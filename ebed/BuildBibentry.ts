import * as fs from 'fs';
import { promisify } from 'util';
import { readdir } from 'fs';

const stat_p = promisify(fs.stat);
const readdir_p = promisify(fs.readdir);
const readFile_p = promisify(fs.readFile);

export interface BibData {
    [key: string]: string;
    sourcefile: string;
    filepos: string;  // will contain a positive number
};

export class BuildBibentry {

    public async addDatafile(datafile: string): Promise<void> {
        await this.loadBibFile(datafile);
    }

    get ids(): string[] {
        return Object.keys(this.list);
    }

    public getDocDescr(extId: string): string {
        return this.buildDocDescr(this.getDocData(extId));
    }

    public getDocData(extId: string): BibData {
        let entry = this.list[extId];

        if (entry === undefined) {
            throw new Error(`Could not get bib info for ${extId}`);
        }

        if (this.enableWarnings) {
            if (entry.warning !== undefined) {
                console.log(`WARNING: ${extId}: ${entry.warning}`);
            }

            let nextChain: string[] = [extId];
            let nextEntry = entry;
            while (nextEntry.next !== undefined) {
                nextChain.push(nextEntry.next);
                nextEntry = this.list[nextEntry.next];
                if (nextEntry === undefined) {
                    throw new Error(`Bibliography: Entry "` +
                        `${nextChain[nextChain.length - 2]}" has next="` +
                        `${nextChain[nextChain.length - 1]}", but that ` +
                        `entry does not exist.`);
                }
            }

            if (nextChain.length > 1) {
                console.log(`WARNING: ${nextChain[0]} has newer versions: ` +
                    `${nextChain.slice(1).join(", ")}`);
            }
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
        return ret.trim();
    }

    private async loadBibFile(file: string): Promise<void> {

        // recursive call if this 'file' is actually a directory:
        if ((await stat_p(file)).isDirectory()) {
            if (file.match(/^\.git$/)) return;
            for (let dirfile of (await readdir_p(file))
                .filter(f => f.endsWith(".bib"))) {
                await this.loadBibFile(file + "/" + dirfile);
            }
            return;
        }

        // this file is not a directory.
        let filedata = await readFile_p(file, 'utf-8');
        let entryRegex = /@\w+\{\s*([^= ,]+)\s*,([^}]+)}/g;

        let match;
        while ((match = entryRegex.exec(filedata)) !== null) {
            this.list[match[1]] =
                this.buildInfoFromBib(match[2], file,
                    entryRegex.lastIndex - match[0].length + 1);
            // console.log(`found ${match[1]}`);
        }
    }

    private buildInfoFromBib(bib: string, filename: string, pos: number)
        : BibData {
        // console.log(bib);
        let elemR = /\s*(\w+)\s*=\s*(?:(?:"((?:\\"|[^"])*[^\\])",?)|([^\s,]+),?)/g;

        let res = elemR.exec(bib);
        let entry: BibData = { sourcefile: filename, filepos: "" + pos };
        while (res) {

            let key = res[1];
            let value = res[2] !== undefined ? res[2] : res[3];

            value = value
                .trim()
                .replace(/[ \t]*(\n)[ \t]*/g, "\n")
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

    private list: { [id: string]: BibData } = {};
    public enableWarnings: boolean = true;
}
