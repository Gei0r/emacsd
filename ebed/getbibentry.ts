import { BuildBibentry, BibData } from './BuildBibentry'

function printUsage() {
    const procname = `${process.argv[0]} ${process.argv[1]}`;
    console.log(`Usage:
${procname} --info  <Bib Database> <entry id>
${procname} --pos   <Bib Database> <entry id>
${procname} --sapid <Bib Database> <entry id>
${procname} --all   <Bib Database>
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

function sapid(argv: string[], builder: BuildBibentry) {
    let data = builder.getDocData(argv[0]);
    if (data.sapid === undefined) process.stdout.write("(no sapid)");
    process.stdout.write(data.sapid.replace(/\/.*/, ""));
}

function saplink(argv: string[], build: BuildBibentry) {
    let data = build.getDocData(argv[0]);
    if (data === undefined) {
        process.stdout.write("(invalid entry)");
        return;
    }
    if (data.sapid === undefined) {
        process.stdout.write("(no sapid)");
        return;
    }

    let match = data.sapid.match(
        /(A6Z\d{11})\/(PM1|PM2|EN1)\/(\w{3})\/(\d{2}|\w|\*|-)/);
    if (!match) {
        process.stdout.write("(invalid sapid)");
        return;
    }

    const document = match[1];
    const typ = match[2];
    const part = match[3];
    const version = match[4];

    const dokopt = version == '*' ? "A" : "T";

    process.stdout.write(`https://p25.transportation.siemens.com/sap/bc/bsp` +
        `/sie/ts_pl03_anonym/default.htm?` +
        `dokar=${typ}&doknr=${document}&` +
        `dokvr=${version}&` +
        `doktl=${part}&` +
        `filenam=&` +
        `dokopt=${dokopt}&` +
        `fileopt=1` +
        `&folddisp=N&` +
        `ph_id=`);
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
                process.argv[2] !== "--all" &&
                process.argv[2] !== "--sapid" &&
                process.argv[2] !== "--saplink" &&
                true)) {
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
        else if (command == "--sapid") sapid(restArgs, builder);
        else if (command == "--saplink") saplink(restArgs, builder);

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
