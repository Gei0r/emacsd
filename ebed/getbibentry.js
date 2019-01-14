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
const BuildBibentry_1 = require("./BuildBibentry");
function main() {
    return __awaiter(this, void 0, void 0, function* () {
        try {
            if (process.argv.length < 4) {
                console.log(`Usage: ${process.argv[0]} ${process.argv[1]} ` +
                    `<datafile> <id>`);
                process.exit(-1);
            }
            let builder = new BuildBibentry_1.BuildBibentry();
            yield builder.addDatafile(process.argv[2]);
            let data = builder.getDocData(process.argv[3]);
            let result = builder.buildDocDescr(data);
            result = result
                .replace(/\n\n/g, "\n")
                .replace(/(^|\n)\s+/g, "$1")
                .replace(/[ \t]{2,}/g, " ")
                .trim();
            let name = data.refid;
            if (name === undefined) {
                name = process.argv[3].replace(/:.*$/, "");
            }
            process.stdout.write(`/${name}/\n`);
            process.stdout.write(result);
        }
        catch (e) {
            console.log(e.stack);
        }
    });
}
main();
