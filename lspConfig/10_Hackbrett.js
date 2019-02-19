
exports.getPlugin = function(locateDominatingFile, getFilesInDirRecursive) {

    /**
       Einstellungen.
    
       Die Einstellungen werden von oben nach unten abgearbeitet.
    
       Es werden die folgenden Daten gesetzt:
    
       incdirs: string[]
         Liste der Include-Pfade, relative zum ClearCase-Root
    
       defines: {[name:string]: string}
         Liste der #defines
    
       flags: string[]
         Weitere flags für clang++.
    
       root: string
         Projektverzeichnis. Wenn es ein relativer Pfad ist, wird es als
         relativ zum ClearCase-Root gewertet.
    
       Wenn der Typ ein Array oder ein Objekt ist, werden die Einträge den
       bestehenden Einträgen hinzugefügt (an das Ende).
       Wenn der Typ number oder string ist, wird der Wert ersetzt.
    
       Wenn das Objekt eine Funktion ist, wird diese aufgerufen. Die Funktion
       bekommt den alten Wert übergeben und muss den neuen Wert zurückgeben.
    */
    const config = {
        "Hackbrett/_inc": {
            root: "Hackbrett/_inc",
            incdirs: ["Hackbrett/_inc", "Hackbrett/_inc/appl",
                "Hackbrett/_inc/devl",
                "Hackbrett/_tools/386/include"],
            flags: ["-Wall", "-Wno-microsoft-cast",
                "-nostdinc++", "-nostdinc"],
        },

        "Hackbrett/_section": {
            incdirs: ["Hackbrett/_inc", "Hackbrett/_inc/appl",
                "Hackbrett/_inc/devl",
                "Hackbrett/_tools/386/include"],
            defines: { "far": "", "FAR": "", "near": "", "NEAR": "" },
            flags: ["-Wall", "-Wno-microsoft-cast",
                "-nostdinc++", "-nostdinc",
                `--include${__dirname.replace(/\\/g, "/")}/CADUL-defs.h`],
            root: (_unused, file) =>
                path.parse(locateDominatingFile(file, "_make")).dir
        },

        "Hackbrett/_section/cu-boards/ecc/devEcccu": {
            root: "Hackbrett/_section/cu-boards/ecc/devEcccu",
            incdirs: ["Hackbrett/_section/cu-boards/ecc/devEcccu/_inc"]
        },

        "Hackbrett/_section/devices/file/_test_src": {
            defines: { "TESTRUNNER_NO_STL": "1" }
        },

        "Hackbrett/_section/kernel32": {
            incdirs: ["Hackbrett/_section/kernel32/_inc"],
            root: "Hackbrett/_section/kernel32"
        },

        "Hackbrett/_section/kernel32/aktualisierung": {
            root: "Hackbrett/_section/kernel32/aktualisierung",
            incdirs: [
                "Hackbrett/_section/kernel32/aktualisierung/_src",
                "Hackbrett/_section/kernel32/tasker/taskercontrol/_src"
            ]
        },

        "Hackbrett/_section/devices/file": {
            defines: { "THROW_NOTHING": "throw" }
        }
    };

    const fs = require('fs');
    const path = require('path');

    async function getRoot(filename, logfile) {
        let hackbrett = locateDominatingFile(filename, "Hackbrett");
        if (hackbrett === undefined) return undefined;

        fs.writeSync(logfile, `Found Hackbrett at ${hackbrett}`);

        let config = loadConfig(hackbrett, filename, logfile);

        fs.writeSync(logfile, `--get-root returning ${config.root}`);
        return config.root;

    }

    async function getCCJson(rootDir, logfile) {
        let hackbrett = locateDominatingFile(rootDir, "Hackbrett");
        if (hackbrett === undefined) return undefined;

        fs.writeSync(logfile, `Found Hackbrett at ${hackbrett}\n`);

        let cppFiles =
            getFilesInDirRecursive(rootDir,
                d => d !== ".ccls-cache" && d !== "_scratch",
                f => f.endsWith(".cpp"));

        return cppFiles.map(cppFile =>
            buildCompileCommandsForFile(hackbrett,
                cppFile, logfile));
    }

    function loadConfig(hackbrett, file, logfile) {
        file = path.resolve(file).replace(/\\/g, "/");

        let resolvedConfig = {};  // return value
        resolvedConfig.CCRoot = path.resolve(hackbrett + "/..")
            .replace(/\\/g, "/");
        resolvedConfig.pathInCC =
            file.substr(resolvedConfig.CCRoot.length + 1);

        for (let fileKey of Object.keys(config)) {
            if (!resolvedConfig.pathInCC.toLowerCase()
                .startsWith(fileKey.toLowerCase())) {
                continue;
            }

            for (let conf of Object.keys(config[fileKey])) {
                let newOption = config[fileKey][conf];
                if (resolvedConfig[conf] === undefined) {
                    // new configuration option

                    if (typeof (newOption) === 'function') {
                        resolvedConfig[conf] = newOption(undefined, file);
                    } else {
                        resolvedConfig[conf] = newOption;
                    }

                } else {
                    // merge options
                    if (typeof (newOption) === 'function') {
                        resolvedConfig[conf] =
                            newOption(resolvedConfig[conf], file);
                    } else if (typeof (newOption) === 'string') {
                        resolvedConfig[conf] = newOption;
                    } else if (newOption instanceof Array) {
                        resolvedConfig[conf] =
                            resolvedConfig[conf].concat(newOption);
                    } else {  // object
                        Object.assign(resolvedConfig[conf], newOption);
                    }
                }
            }
        }

        // if (logfile) {
        //     fs.writeSync(logfile, JSON.stringify(resolvedConfig, null, 2));
        //     fs.writeSync(logfile, "\n");
        // }

        if (resolvedConfig.root === undefined) resolvedConfig.root = "";
        if (!path.isAbsolute(resolvedConfig.root)) {
            resolvedConfig.root =
                path.resolve(`${resolvedConfig.CCRoot}/${resolvedConfig.root}`)
                    .replace(/\\/g, "/");
        }
        return resolvedConfig;
    }

    function buildCompileCommandsForFile(hackbrett, cppfile, logfile) {
        let ret = {};

        let cnf = loadConfig(hackbrett, cppfile, logfile);

        ret.file = cppfile;
        ret.directory = path.isAbsolute(cnf.root) ?
            cnf.root : `${cnf.CCRoot}/${cnf.root}`;
        ret.command = "clang++ ";

        if (cnf.defines !== undefined) {
            let defs = [];
            for (let key of Object.keys(cnf.defines)) {
                let val = cnf.defines[key];
                if (val === "") {
                    defs.push(`-D${key}=`);
                } else {
                    defs.push(`-D${key}=${val}`);
                }
            }
            ret.command += defs.join(" ") + " ";
        }

        if (cnf.flags !== undefined) {
            ret.command += cnf.flags.join(" ") + " ";
        }

        if (cnf.incdirs !== undefined) {
            ret.command +=
                cnf.incdirs.map(d => `-I${cnf.CCRoot}/${d}`).join(" ") + " ";
        }

        ret.command += cppfile;

        return ret;
    }

    return { getRoot: getRoot, getCCJson: getCCJson };
}
