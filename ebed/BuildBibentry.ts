import * as fs from 'fs';
import { promisify } from 'util';
import { readdir } from 'fs';

const stat_p = promisify(fs.stat);
const readdir_p = promisify(fs.readdir);
const readFile_p = promisify(fs.readFile);

export type BibData = { [key: string]: string };

export class BuildBibentry {

    public async addDatafile(datafile: string): Promise<void> {
        await this.loadBibFile(datafile);
    }

    public getDocDescr(extId: string): string {
        return this.buildDocDescr(this.getDocData(extId));
    }

    public getDocData(extId: string): BibData {
        let entry = this.list[extId];

        if (entry === undefined) {
            throw new Error(`Could not get bib info for ${extId}`);
        }

        // console.log(`${extId} => ${JSON.stringify(entry)}\n`);

        return entry;
    }

    public buildDocDescr(entry: BibData): string {
        let ret = "";

        if (entry.type) ret += entry["type"] + "\n\n";
        if (entry.title) ret += entry["title"] + "\n\n";
        if (entry.subtitle) ret += entry["subtitle"] + "\n\n";
        if (entry.scn) ret += `SSN: ${entry.scn}\n\n`;
        if (entry.note) ret += entry.note + "\n\n";
        if (entry.sapid) ret += `SAP-ID: ${entry.sapid}\n\n`;
        if (entry.sig) ret += `Signatur: ${entry.sig}\n\n`;
        if (entry.notes) ret += `(${entry.notes})\n\n`;
        return ret;
    }

    private async loadBibFile(file: string): Promise<void> {

        // recursive call if this 'file' is actually a directory:
        if ((await stat_p(file)).isDirectory()) {
            if (file.match(/\.git$/)) return;
            for (let dirfile of (await readdir_p(file))) {
                await this.loadBibFile(file + "/" + dirfile);
            }
            return;
        }

        // this file is not a directory.
        let filedata = await readFile_p(file, 'utf-8');
        let entryRegex = /@\w+\{\s*([^= ,]+)\s*,([^}]+)}/g;

        let match;
        while ((match = entryRegex.exec(filedata)) !== null) {
            this.list[match[1]] = this.buildInfoFromBib(match[2]);
            // console.log(`found ${match[1]}`);
        }
    }

    private buildInfoFromBib(bib: string): BibData {
        // console.log(bib);
        let elemR = /\s*(\w+)\s*=\s*(?:(?:"((?:\\"|[^"])*[^\\])",?)|([^\s,]+),?)/g;

        let res = elemR.exec(bib);
        let entry: { [key: string]: string } = {};
        while (res) {

            let key = res[1];
            let value = res[2] !== undefined ? res[2] : res[3];

            value = value
                .replace(/\\"/g, '"')
                .replace(/---/g, '\u2014')
                .replace(/--/g, '\u2013')
                .replace(/~/g, '\u00a0')
                .replace(/\n/g, "\n\n");


            entry[key] = value;
            res = elemR.exec(bib);
        }

        // console.log(JSON.stringify(entry));

        return entry;
    }

    private datafiles: string[] = [];
    private list: { [id: string]: BibData } = {};
}
