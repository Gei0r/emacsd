# Adrian's emacs configuration

- Familiar keybindings for Windows users (e.g. Ctrl+O = open, Ctrl+S = save,
  etc.)
- Many useful packages and features for base system development

## Software to preinstall

For the best experience, make sure these are in the `PATH`:
- `node`
  ([download from official website](https://nodejs.org/en/download/prebuilt-installer))
- `clangd` (for C++ development,
  [download from GitHub](https://github.com/clangd/clangd/releases/latest))
- `DocBuilder` (for syntax check in DocBuilder,
  [download from code.siemens.com](https://code.siemens.com/umatch/general-tools/docbuilder/-/jobs/artifacts/master/raw/bin/DocBuilder.exe?job=run))

## How to use

Clone this repo to `%appdata\.emacs.d`:

```
git clone git@code.siemens.com:adrian.ebeling/emacsd.git %appdata%\.emacs.d
```

You can also use a different path by setting the `HOME` environment variable.
This has the added benefit that `~` within emacs will point to this directory.

On Linux, just clone to `~/.emacs.d`.
