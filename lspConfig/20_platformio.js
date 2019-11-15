
exports.getPlugin = function(locateDominatingFile, getFilesInDirRecursive) {

    const path = require('path');
    const fs = require('fs');

    async function getRoot(filename, logfile) {
        let pathPlatformioIni =
            locateDominatingFile(filename, "platformio.ini");
        if (pathPlatformioIni === undefined) return undefined;

        let projRoot = path.parse(pathPlatformioIni).dir;
        fs.writeSync(logfile, `found platformio.ini in ${projRoot}\n`);
        return projRoot;
    }

    function makeLibraryIncludeDirs(libdir) {
        let ret = [];

        let dirs = [];
        try {
            dirs = fs.readdirSync(libdir, { withFileTypes: true })
                .filter(e => e.isDirectory())
                .map(d => d.name);
        } catch (e) {
            // ignore non-existing dir
            return ret;
        }

        for (let dir of dirs) {
            let thisLibDir = `${libdir}/${dir}`;
            ret.push(thisLibDir);

            // if this library dir contains a "src" subdir, also add this to
            // ret.
            try {
                if (fs.statSync(`${thisLibDir}/src`).isDirectory()) {
                    ret.push(`${thisLibDir}/src`);
                }
            } catch (e) { /*ignore*/ }

            ret.push(thisLibDir);
        }

        return ret;
    }

    function buildCCForFile(rootdir, cppfile, includePaths, cflags) {
        let ret = {};

        ret.file = cppfile;
        ret.directory = rootdir;
        ret.command = "clang++ ";
        ret.command += includePaths.map(p => `-I${p}`).join(" ") + " ";
        ret.command += cflags.join(" ") + " ";
        ret.command += cppfile;

        return ret;
    }

    async function getCCJson(rootDir, logfile) {

        // if there is no platformio.ini file in rootDir, this is not a pio
        // project.
        try {
            fs.accessSync(rootDir + "/platformio.ini", fs.constants.R_OK);
        } catch (e) {
            // no platformio.ini
            return undefined;
        }

        let pioHomeDir;
        if (process.platform === "win32") {
            if (process.env.PIOHOME !== undefined) {
                pioHomeDir = process.env.PIOHOME;
            } else {
                pioHomeDir =
                    path.resolve("C:/" + process.env.HOMEPATH + "/.platformio")
                        .replace(/\\/g, "/");
            }
        } else {
            pioHomeDir = path.resolve(process.env.HOME + "/.platformio");
        }
        fs.writeSync(logfile, "pioHomeDir: " + pioHomeDir + "\n");

        let frBase = `${pioHomeDir}/packages/framework-arduinoespressif8266`;
        let tcBase = `${pioHomeDir}/packages/toolchain-xtensa`;
        let tcInc = `${tcBase}/xtensa-lx106-elf/include/c++/4.8.2`;
        let includePaths = [
            `${tcInc}`,
            `${tcInc}/xtensa-lx106-elf`,
            `${tcInc}/backward`,
            `${tcBase}/lib/gcc/xtensa-lx106-elf/4.8.2/include`,
            `${tcBase}/lib/gcc/xtensa-lx106-elf/4.8.2/include-fixed`,
            `${tcBase}/xtensa-lx106-elf/include`,

            ...makeLibraryIncludeDirs(`${frBase}/libraries`),

            `${frBase}/tools/sdk/include`,
            `${frBase}/tools/sdk/libc/xtensa-lx106-elf/include`,
            `${frBase}/cores/esp8266`,
            `${frBase}/tools/sdk/lwip2/include`,
            `${frBase}/variants/d1_mini`,

            ...makeLibraryIncludeDirs(`${rootDir}/.piolibdeps`),
            ...makeLibraryIncludeDirs(`${rootDir}/lib`)
        ];

        let cflags = [
            `--sysroot=${tcBase}/xtensa-lx106-elf/`,
            "-nostdinc",
            "-nostdinc++",

            "-fno-rtti",
            "-fno-exceptions",
            "-std=c++11",
            "-Os",
            "-ffunction-sections",
            "-fdata-sections",
            "-Wall",
            "-DF_CPU=80000000L",
            "-D__ets__",
            "-DICACHE_FLASH",
            "-DPLATFORMIO=30504",
            "-DESP8266",
            "-DARDUINO_ARCH_ESP8266",
            "-DESP8266_WEMOS_D1MINI",
            "-DPIO_FRAMEWORK_ARDUINO_LWIP2_LOW_MEMORY",
            "-DARDUINO=10805",
            "-DLWIP_OPEN_SRC",
            "-DTCP_MSS=536",
            "-U__STRICT_ANSI__",
            "-fno-ms-extensions",
            "-fno-ms-compatibility",
            "-fno-delayed-template-parsing"
        ];

        // todo: open platformio.ini, get additional build_flags

        fs.writeSync(logfile, `include paths:\n` +
            `${JSON.stringify(includePaths, null, 2)}`);
        fs.writeSync(logfile, `cflags:\n${JSON.stringify(cflags, null, 2)}`);

        debugger;
        let cppFiles = getFilesInDirRecursive(rootDir,
            (_u, d) => d.startsWith(`${rootDir}/src`),
            f => f.endsWith(".cpp"));

        fs.writeSync(logfile, `cppfiles: ${JSON.stringify(cppFiles)}`);

        return cppFiles
            .map(cppFile =>
                buildCCForFile(rootDir, cppFile, includePaths, cflags));
    }

    return { getRoot: getRoot, getCCJson: getCCJson };
}
