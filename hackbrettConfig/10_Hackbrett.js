
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
            isystem: [
                "Hackbrett/_inc", "Hackbrett/_inc/devl", "Hackbrett/_inc/appl",
                "Hackbrett/_tools/386/include",
                "Hackbrett/_inc/ds3_software_mirror/_inc",
                "Hackbrett/_section/ds3/_inc",
            ],
            flags: ["-Wall", "-Wno-microsoft-cast", "-Wno-unknown-pragmas",
                "-nostdinc++", "-nostdinc",
                `--include${__dirname.replace(/\\/g, "/")}/CADUL-defs.h`,
                "-std=c++98", "--target=i386-pc-win32"],
        },

        "Hackbrett/_inc/test": {
            root: "Hackbrett/_inc/test",
            flags: [`--include${__dirname.replace(/\\/g, "/")}/CADUL-defs.h`],
            defines: { far: "", FAR: "", near: "", NEAR: "" },
        },

        "Hackbrett/_section": {
            isystem: [
                "Hackbrett/_inc", "Hackbrett/_inc/appl", "Hackbrett/_inc/devl",
                "Hackbrett/_tools/386/include",
                "Hackbrett/_tools/386/include/iostl",
                "Hackbrett/_tools/386/include/std",
                "Hackbrett/_inc/ds3_software_mirror/_inc",
                "Hackbrett/_section/ds3/_inc",
            ],
            flags: ["-Wall", "-Wno-microsoft-cast",
                "-Wno-unknown-pragmas",
                "-nostdinc++", "-nostdinc",
                `--include${__dirname.replace(/\\/g, "/")}/CADUL-defs.h`,
                "-std=c++98", "--target=i386-pc-win32"],
            root: (_unused, file) =>
                path.parse(locateDominatingFile(file, "_make")).dir
        },

        "Hackbrett/_section/cu-boards/ecc/devEcccu": {
            root: "Hackbrett/_section/cu-boards/ecc/devEcccu",
            incdirs: ["Hackbrett/_section/cu-boards/ecc/devEcccu/_inc",
                "Hackbrett/_section/cu-boards/uecc/inspector/_src"]
        },

        "Hackbrett/_section/Demo": {
            incdirs: ["Hackbrett/_section/comm/Streams/_inc"]
        },

        "Hackbrett/_section/kernel32": {
            incdirs: [
                "Hackbrett/_section/kernel32/_inc",
                "Hackbrett/_section/kernel32/tasker/taskercontrol/_src",
            ],
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
        },

        "Hackbrett/_src/packager": {
            root: "Hackbrett/_src/packager",
        },

        "Hackbrett/_src/what": {
            root: "Hackbrett/_src/what",
        }
    };

    const fs = require('fs');
    const path = require('path');

    async function getRoot(filename, logfile) {
        let hackbrett = locateDominatingFile(filename, "Hackbrett");
        if (hackbrett === undefined) return undefined;

        fs.writeSync(logfile, `Found Hackbrett at ${hackbrett}\n`);

        let config = loadConfig(hackbrett, filename, logfile);

        fs.writeSync(logfile, `--get-root returning ${config.root}\n`);
        return config.root;
    }

    async function writeClangd(file, logfile) {
        let hackbrett = locateDominatingFile(file, "Hackbrett");
        if (hackbrett === undefined) return undefined;

        fs.writeSync(logfile, `Found Hackbrett at ${hackbrett}\n`);

        let ret = { CompileFlags: { Add: [] } };
        let flags = ret.CompileFlags.Add;

        let cnf = loadConfig(hackbrett, file, logfile);

        if (cnf.defines !== undefined) {
            for (let key of Object.keys(cnf.defines)) {
                let val = cnf.defines[key];
                if (val === "") {
                    flags.push(`-D${key}=`);
                } else {
                    flags.push(`-D${key}=${val}`);
                }
            }
        }

        if (cnf.flags !== undefined) {
            flags.push(...cnf.flags);
        }

        if (cnf.incdirs !== undefined) {
            flags.push(...cnf.incdirs.map(d => `-I${cnf.CCRoot}/${d}`));
        }

        if (cnf.isystem !== undefined) {
            flags.push(...cnf.isystem.map(d => `-isystem${cnf.CCRoot}/${d}`));
        }

        const clangdFile = cnf.root + "/.clangd";
        if (fs.existsSync(clangdFile)) {
            fs.writeSync(logfile, `NOT writing ${clangdFile} (file exists)\n`);
        } else {
            fs.writeSync(logfile, `Writing ${clangdFile}\n`);
            fs.writeFileSync(clangdFile, JSON.stringify(ret, null, 2),
                { encoding: 'utf8' });
        }

        return ret;
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

        if (resolvedConfig.root !== undefined &&
            !path.isAbsolute(resolvedConfig.root)) {
            resolvedConfig.root =
                path.resolve(`${resolvedConfig.CCRoot}/` +
                    `${resolvedConfig.root}`)
                    .replace(/\\/g, "/");
        }

        return resolvedConfig;
    }

    return { getRoot: getRoot, writeClangd: writeClangd };
}
