/**
  Adrian Ebeling 22 Jan 2019

  Configuration for LSP (language server protocol) and ccls [1].

  This command provides two functions which are reachable from the command
  line:

  1. Input:  Source file name (e.g. *.cpp, *.h)
     Output: Project root directory

     node lspConfig.js --get-root <filename>

  2. Input: Project root directory
     Output: compile_commands.json

     node lspConfig.js <root dir>

  All input is is through command line arguments.
  All output is through stdout.

  *This* script doesn't do any processing; it is handled by additional (plugin)
  files which must be in the same directory.
  Each plugin file (*.js) must export two functions:

  - async function getRoot(filename: string): string|undefined
  - async function getCCJson(rootDir: string): object|undefined

  Additionally, the following things are passed to both functions, in order:
  - logfile: number A file descriptor of an opened file that can be used with
    fs.writeSync().
  - locateDominatingFile: Helper function, see below.
    Similar to emacs' locate-dominating-file, but the whole filename is
    returned.
  - readFilesInDirRecursive: Helper function, see comment below.

  Plugins are called in alphabetical order.
*/

const fs = require('fs');
const path = require('path');
const logfilename = process.platform == 'win32' ?
    "D:/temp/lspConfig.log" : "/tmp/lspConfig.log";

async function main() {
    let logfile = undefined;
    try {
        // open logfile
        logfile = fs.openSync(logfilename, 'a');
        fs.writeSync(logfile, '\n\n----------------------------\n');
        fs.writeSync(logfile, `argv: ${JSON.stringify(process.argv)}\n`);
        fs.writeSync(logfile, `cwd: ${process.cwd()}\n`);

        const getRoot = (process.argv[2] === "--get-root");

        let jsFiles = fs.readdirSync(__dirname, { withFileTypes: true })
            .filter(e => e.isFile() &&
                e.name.match(/\.js$/) &&
                e.name !== "lspConfig.js")
            .map(e => e.name)
            .sort();

        // go through all js files in this directory in order:
        for (let file of jsFiles) {
            file = `${__dirname}/${file}`;
            let exp = require(file);

            if (exp.getPlugin === undefined) {
                fs.writeSync(logfile, `Could not load ${file}\n`);
                continue;
            }

            let plugin = exp.getPlugin(locateDominatingFile,
                readFilesInDirRecursive);

            if (getRoot) {
                let result = await plugin.getRoot(process.argv[3], logfile);
                if (result !== undefined) {
                    fs.writeSync(logfile, `get-root result: ${result}`);
                    console.log(result);
                    break;
                }
            } else {
                let projDir =
                    path.resolve(process.argv[2]).replace(/\\/g, "/");
                let result = await plugin.getCCJson(projDir, logfile);
                if (result !== undefined) {
                    const resultJson = JSON.stringify(result, null, 2);
                    fs.writeSync(logfile, `result: ${resultJson}`);
                    process.stdout.write(resultJson);
                    break;
                }
            }
        }
    } catch (e) {
        process.stderr.write(e.stack);
        if (logfile !== undefined) {
            fs.writeSync(logfile, e.stack);
        }
    }
}

/**
   Starting at FILE, look up directory hierarchy for file matching NAME.
   FILE can be a file or a directory.  If it’s a file, its directory will
   serve as the starting point for searching the hierarchy of directories.
   Stop at the first parent directory containing a file NAME,
   and return the file name.  Return undefined if not found.
   Instead of a string, NAME can also be a predicate taking one argument
   (a directory) and returning true if that directory is the one for
   which we’re looking.  The predicate will be called with every file/directory
   the function needs to examine, starting with FILE.
 */
function locateDominatingFile(file, name) {
    // if name is a string, transform it to a predicate.
    if (typeof name === "string") {
        let nameStr = name;
        name = s => s === nameStr;
    }

    if (file === undefined) file = process.cwd();
    file = path.resolve(file).replace(/\\/g, "/");

    let isDir = false;
    try {
        isDir = fs.statSync(file).isDirectory();
    } catch (e) { /* doesn't exist --> not a dir either */ }

    if (!isDir) {
        // start at the file's parent directory instead
        file = path.parse(file).dir;
    }

    // apply the predicate to each file in file, filtering out the ones where
    // the predicate returned true.
    let matchingFiles = fs.readdirSync(file).filter(name);

    // found a matching file!
    if (matchingFiles.length > 0) {
        return file + "/" + matchingFiles[0];
    }

    // not found, check parent directory
    let parent = path.resolve(file + "/..").replace(/\\/g, "/");
    if (parent == file) {
        // reached root, not found
        return undefined;
    }

    // recursive call
    return locateDominatingFile(parent, name);
}

/**
   Recursively gets all the files in the directory

   @param dir: string Starting directory
   @param dirPred : (filename, fullpath) => bool (optional)
          Directories are only entered if dirPred returns true
   @param filePred : (filename, fullpath) => bool (optional)
          Files are only returned if filePred returns true
   @return Array of filenames (full paths)
*/
function readFilesInDirRecursive(dir, dirPred, filePred) {
    if (dirPred === undefined) dirPred = () => true;
    if (filePred === undefined) filePred = () => true;

    dir = path.resolve(dir).replace(/\\/g, "/");

    let files = fs.readdirSync(dir, { withFileTypes: true });
    let ret = [];

    for (let file of files) {
        let fullFileName = `${dir}/${file.name}`;
        if (file.isDirectory() && dirPred(file.name, fullFileName)) {
            // recursive call
            ret.push(...readFilesInDirRecursive(`${dir}/${file.name}`,
                dirPred, filePred));
        } else if (file.isFile() && filePred(file.name, fullFileName)) {
            ret.push(`${dir}/${file.name}`);
        }
    }

    return ret;
}

main();
