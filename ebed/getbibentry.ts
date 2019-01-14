import { BuildBibentry } from './BuildBibentry'

async function main() {
    try {

        if (process.argv.length < 4) {
            console.log(`Usage: ${process.argv[0]} ${process.argv[1]} ` +
                `<datafile> <id>`);
            process.exit(-1);
        }

        let builder = new BuildBibentry();

        await builder.addDatafile(process.argv[2]);

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
        // process.stdout.write(`/${name}/\n`);
        process.stdout.write(result);
    } catch (e) {
        console.log(e.stack);
    }

}

main();
