Status: [![Haskell CI](https://github.com/fmidue/haskell-template-parametrization/workflows/Haskell%20CI/badge.svg)](https://github.com/fmidue/haskell-template-parametrization/actions?query=workflow%3A%22Haskell+CI%22+branch%3Amain)

## Usage guidelines

- [Download](https://docs.haskellstack.org/en/stable/install_and_upgrade/) and install `stack`
- Clone this repository (`git clone https://github.com/fmidue/haskell-template-parametrization.git`).
- Build it from the cloned directory with `stack build`.
- Use `stack exec generate-tasks "path-to-folder"` to translate tasks.

The folder to be translated should have the following contents:

- `defaults.hs` file with the default vars
- `/tasks` folder with all tasks
- `/solutions` folder with all solutions

The solutions should have the same name as the tasks they are belonging to.

As output, two folders `output/tasks` and `output/solutions` fill be created that contain the materializations of what was provided as input.
