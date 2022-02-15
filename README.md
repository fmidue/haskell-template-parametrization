Status: [![Haskell CI](https://github.com/fmidue/ba-leon-koth/workflows/Haskell%20CI/badge.svg)](https://github.com/fmidue/ba-leon-koth/actions?query=workflow%3A%22Haskell+CI%22+branch%3Amaster)

## Usage guidelines

- [Download](https://docs.haskellstack.org/en/stable/install_and_upgrade/) and install stack
- Clone this repository (git clone https://github.com/fmidue/ba-leon-koth.git)
- Build it from the cloned directory with `stack build`
- Use `stack exec generate-tasks "path-to-folder"` to translate tasks

## Folder contents
The folder to be translate should have the following contents:
- `defaults.hs` file with the default vars
- `/tasks` folder with all tasks
- `/solutions` folder with all solutions

The solutions should have the same name as the tasks they are belonging to.


