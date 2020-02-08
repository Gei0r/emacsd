"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const fs = require("fs");
const util_1 = require("util");
const stat_p = util_1.promisify(fs.stat);
const readdir_p = util_1.promisify(fs.readdir);
const readFile_p = util_1.promisify(fs.readFile);
;
class BuildBibentry {
    constructor() {
        this.list = {};
        this.enableWarnings = true;
    }
    addDatafile(datafile) {
        return __awaiter(this, void 0, void 0, function* () {
            yield this.loadBibFile(datafile);
        });
    }
    get ids() {
        return Object.keys(this.list);
    }
    getDocDescr(extId) {
        return this.buildDocDescr(this.getDocData(extId));
    }
    getDocData(extId) {
        let entry = this.list[extId];
        if (entry === undefined) {
            throw new Error(`Could not get bib info for ${extId}`);
        }
        if (this.enableWarnings) {
            if (entry.warning !== undefined) {
                console.log(`WARNING: ${extId}: ${entry.warning}`);
            }
            let nextChain = [extId];
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
    buildDocDescr(entry) {
        let ret = "";
        if (entry.type)
            ret += entry["type"] + "\n\n";
        if (entry.title)
            ret += entry["title"] + "\n\n";
        if (entry.subtitle)
            ret += entry["subtitle"] + "\n\n";
        if (entry.scn)
            ret += `SSN: ${entry.scn}\n\n`;
        if (entry.note)
            ret += entry.note + "\n\n";
        if (entry.sapid)
            ret += `SAP-ID: ${entry.sapid}\n\n`;
        if (entry.sig)
            ret += `Signatur: ${entry.sig}\n\n`;
        if (entry.notes)
            ret += `(${entry.notes})\n\n`;
        return ret.trim();
    }
    loadBibFile(file) {
        return __awaiter(this, void 0, void 0, function* () {
            // recursive call if this 'file' is actually a directory:
            if ((yield stat_p(file)).isDirectory()) {
                if (file.match(/^\.git$/))
                    return;
                for (let dirfile of (yield readdir_p(file))
                    .filter(f => f.endsWith(".bib"))) {
                    yield this.loadBibFile(file + "/" + dirfile);
                }
                return;
            }
            // this file is not a directory.
            let filedata = yield readFile_p(file, 'utf-8');
            let entryRegex = /@\w+\{\s*([^= ,]+)\s*,([^}]+)}/g;
            let match;
            while ((match = entryRegex.exec(filedata)) !== null) {
                this.list[match[1]] =
                    this.buildInfoFromBib(match[2], file, entryRegex.lastIndex - match[0].length + 1);
                // console.log(`found ${match[1]}`);
            }
        });
    }
    buildInfoFromBib(bib, filename, pos) {
        // console.log(bib);
        let elemR = /\s*(\w+)\s*=\s*(?:(?:"((?:\\"|[^"])*[^\\])",?)|([^\s,]+),?)/g;
        let res = elemR.exec(bib);
        let entry = { sourcefile: filename, filepos: "" + pos };
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
}
exports.BuildBibentry = BuildBibentry;
