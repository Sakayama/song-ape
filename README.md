# Song-Ape

This app is able to work with songs, namely ordering songs, albums, etc.

## How To Run

[Haskell Stack](https://docs.haskellstack.org/) install is required

- `stack build --file-watch --exec song-ape-exe` installs dependencies (if necessary), builds a project (watches file changes), and runs it
- `stack exec --whereis song-ape-exe` locates an executable file

## How to run frontend

```bash
cd webapp

rm -rf "./build" &&
mkdir "build" &&
cp "./src/index.html" "./build" &&
cp "./src/style.css" "./build" &&
cp -r "./src/icons" "./build" &&
cp -r "./src/images" "./build" &&
elm-live src/Main.elm --dir="./build" --open -- --output="./build/bundle.js"

```

## Assumptions

- Each album has a cover url and a description.
- Each artist has a photo url and a description.
