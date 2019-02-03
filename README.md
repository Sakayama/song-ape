# Song-Ape

This app is able to work with songs, namely ordering songs, albums, etc.

## How To Run

[Haskell Stack](https://docs.haskellstack.org/) install is required
- `stack build --file-watch --exec song-ape-exe` installs dependencies (if necessary), builds a project (watches file changes), and runs it
- `stack exec --whereis song-ape-exe` locates an executable file