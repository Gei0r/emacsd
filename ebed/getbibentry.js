"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const BuildBibentry_1 = require("./BuildBibentry");
function printUsage() {
    const procname = `${process.argv[0]} ${process.argv[1]}`;
    console.log(`Usage:
${procname} --info  <Bib Database> <entry id>
${procname} --pos   <Bib Database> <entry id>
${procname} --sapid <Bib Database> <entry id>
${procname} --all   <Bib Database>
`);
}
function info(argv, builder) {
    if (argv.length < 1) {
        throw new Error('Usage');
    }
    const id = argv[0];
    let data = builder.getDocData(id);
    let result = builder.buildDocDescr(data);
    result = result
        .replace(/\n\n/g, "\n")
        .replace(/(^|\n)\s+/g, "$1")
        .replace(/[ \t]{2,}/g, " ")
        .trim();
    let name = data.refid;
    if (name === undefined) {
        name = id.replace(/:.*$/, "");
    }
    process.stdout.write(`/${name}/\n`);
    process.stdout.write(result);
}
function sapid(argv, builder) {
    let data = builder.getDocData(argv[0]);
    if (data.sapid === undefined)
        process.stdout.write("(no sapid)");
    process.stdout.write(data.sapid.replace(/\/.*/, ""));
}
function pos(argv, builder) {
    if (argv.length < 1) {
        throw new Error('Usage');
    }
    let data = builder.getDocData(argv[0]);
    process.stdout.write(data.sourcefile + "@" + data.filepos);
}
function all(builder) {
    let result = [];
    for (let id of builder.ids) {
        let data = builder.getDocData(id);
        data.id = id;
        result.push(data);
    }
    console.log(JSON.stringify(result));
}
function main() {
    return __awaiter(this, void 0, void 0, function* () {
        try {
            if (process.argv.length < 4 ||
                (process.argv[2] !== "--info" &&
                    process.argv[2] !== "--pos" &&
                    process.argv[2] !== "--all" &&
                    process.argv[2] !== "--sapid")) {
                throw new Error('Usage');
            }
            let command = process.argv[2];
            let datafile = process.argv[3];
            let restArgs = process.argv.slice(4);
            let builder = new BuildBibentry_1.BuildBibentry();
            yield builder.addDatafile(datafile);
            builder.enableWarnings = false;
            if (command == "--info")
                info(restArgs, builder);
            else if (command == "--pos")
                pos(restArgs, builder);
            else if (command == "--all")
                all(builder);
            else if (command == "--sapid")
                sapid(restArgs, builder);
        }
        catch (e) {
            if (e.message === "Usage") {
                printUsage();
                process.exit(-1);
            }
            else {
                process.stderr.write(e.message);
                process.exit(-1);
            }
        }
    });
}
main();
