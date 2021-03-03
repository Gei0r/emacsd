import { BuildBibentry, BibData } from './BuildBibentry'

function printUsage() {
    const procname = `${process.argv[0]} ${process.argv[1]}`;
    console.log(`Usage:
${procname} --info <Bib Database> <entry id>
${procname} --pos  <Bib Database> <entry id>
${procname} --all  <Bib Database>
`);
}

function info(argv: string[], builder: BuildBibentry) {
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

function pos(argv: string[], builder: BuildBibentry) {
    if (argv.length < 1) {
        throw new Error('Usage');
    }

    let data = builder.getDocData(argv[0]);
    process.stdout.write(data.sourcefile + "@" + data.filepos);
}

function all(builder: BuildBibentry) {
    let result: BibData[] = [];

    for (let id of builder.ids) {
        let data = builder.getDocData(id);
        data.id = id;
        result.push(data);
    }

    console.log(JSON.stringify(result));
}

async function main() {
    try {
        if (process.argv.length < 4 ||
            (process.argv[2] !== "--info" &&
                process.argv[2] !== "--pos" &&
                process.argv[2] !== "--all")) {
            throw new Error('Usage');
        }

        let command = process.argv[2];
        let datafile = process.argv[3];
        let restArgs = process.argv.slice(4);

        let builder = new BuildBibentry();
        await builder.addDatafile(datafile);
        builder.enableWarnings = false;

        if (command == "--info") info(restArgs, builder);
        else if (command == "--pos") pos(restArgs, builder)
        else if (command == "--all") all(builder);

    } catch (e) {
        if (e.message === "Usage") {
            printUsage();
            process.exit(-1);
        } else {
            process.stderr.write(e.message);
            process.exit(-1);
        }
    }

}

main();
