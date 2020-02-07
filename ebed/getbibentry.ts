import { BuildBibentry } from './BuildBibentry'

async function main() {
    try {

        if (process.argv.length < 5 ||
            (process.argv[2] !== "--info" && process.argv[2] !== "--pos")) {
            console.log(`Usage: ${process.argv[0]} ${process.argv[1]} ` +
                '<--info | --pos> ' +
                `<datafile> <id>`);
            process.exit(-1);
        }

        let builder = new BuildBibentry();

        let what = process.argv[2];
        let datafile = process.argv[3];
        let id = process.argv[4];

        await builder.addDatafile(datafile);

        let data = builder.getDocData(id);

        if (what == "--info") {
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
        else if (what == "--pos") {
            process.stdout.write(data.sourcefile + "@" + data.filepos);
        }
    } catch (e) {
        console.log(e.stack);
    }

}

main();
