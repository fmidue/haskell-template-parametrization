Monitor this badge, once there is some real Haskell code in the repo: [![Haskell CI](https://github.com/fmidue/ba-leon-koth/workflows/Haskell%20CI/badge.svg)](https://github.com/fmidue/ba-leon-koth/actions?query=workflow%3A%22Haskell+CI%22+branch%3Amaster)

# General thesis guidelines

["Richtlinien für Abschlussarbeiten" der Gruppe von Prof. Fuhr](https://www.is.inf.uni-due.de/wiki/index.php/Richtlinien_f%c3%bcr_Abschlussarbeiten) - to a large extent applicable for us as well

# Some development guidelines

The following are slogans from Agile philosophy. Not all of them should be followed slavishly, but there is certain truth to them. Maybe google for them and read a bit.

- Commits should be frequent, small and atomic.

- If you can't commit before going home, revert.

- Never break the `master` build.

- Die, long-lived branches!

Commit messages should be informative (instead of "updated Types.hs", rather "added a tree datatype").

In Haskell specifically:

- In probably more than 90% of occurrences, use of `fromJust` is a mistake (i.e., is bad coding).

- Using `nub` is always never a good idea. Use `nubOrd` (or even `nubSort`) instead.

- Writing your own recursions on lists is very rarely necessary. Usually, list comprehensions or existing (higher-order) functions, or combinations thereof, are a better fit.

- Make use of [https://hoogle.haskell.org/](https://hoogle.haskell.org/) to search for existing functionality (by name or by type) instead of implementing all kinds of auxiliary functions yourself. Specifically, the [`extra` package](https://hackage.haskell.org/package/extra) is a good source of helper functions.

- Make conscious decisions about using `Int` vs. using `Integer`.

- Mostly avoid introducing type synonyms via `type`. Instead, use `data` or `newtype` where appropriate.

- Use explicit export lists. Make them as short as possible in each module.

- Mostly use explicit import lists. Read [https://wiki.haskell.org/Import_modules_properly](https://wiki.haskell.org/Import_modules_properly) and consider their advice.

- Use HLint.

- Use `ghc -Wall`.

- Use `stack test --coverage; stack hpc report --all --open`.

Generally, when structuring your code base, think about (and if necessary, first google for) "cohesion and (loose) coupling".

For test suites, always consider writing also negative test cases.

# More general Agile concepts

You may also want to read up on some other notions and techniques from Agile software engineering. Such as:

- Timeboxing

- Minimum Viable Product

- Kanban/Scrum Boards

- Definition of Done

# About reporting problems while working on code

Always in this way:

- concrete code is committed and pushed to repo (maybe in a separate branch only meant for reporting the problem),

- an issue is opened, in which the problem ("code from branch/commit so-and-so exhibits ...") is explained:

- problem / error message / desired result.

Also, whenever possible, provide an [SSCCE](http://sscce.org/).

# About git and GitHub use

- If you have not worked with `git` before, and are a Windows or macOS user, consider installing and using [GitHub Desktop](https://desktop.github.com/). Some of the CLI instructions below are then less relevant for you. In any case, always work with a proper repository set up on your local machine, not with a disconnected local directory and manual uploads via the GitHub web interface!

- Run `git config --global pull.rebase true` or `git config --local pull.rebase true` in the relevant clone of the repo, once up front.

- Use the power of `git add -p` or an equivalent "hunk staging" functionality in your Git client.

- Before each `git commit`, execute `git diff --staged --check` (or an equivalent workflow in a non-CLI Git client).

- On GitHub, e.g., in issues and comments, use GitHub Markup for code.

- Where appropriate (possibly on GitHub and in Slack), make use of [GitHub permalinks](https://stackoverflow.com/questions/23821235/how-to-link-to-specific-line-number-on-github/50463252#50463252).

- Possibly use GitHub project boards in the repo, in the spirit of Kanban/Scrum or elsewise.

# On writing the actual thesis

- Start writing almost immediately after the work has started, recording ideas, concepts, initial developments, etc.

- Use LaTeX and appropriate packages (e.g., for diagram visualizations).

- Put the LaTeX source code under version control in the same GitHub repo as the program code being developed.

- Use a spell checker! Always! Ideally in on-the-fly mode.

# On Slack use

- Besides GitHub Issues, possibly also use the in-Slack bookmarks feature to keep track of things you have to do, react to, etc.

- When starting a thread by answering one's own (or GitHub's) message, it is possible that nobody else notices the new one, due to the way Slack notifications work. Better then to use the "also send to channel" option.

# Updates to this README

Take a look at [https://fmidue.github.io/ba-ma-template/](https://fmidue.github.io/ba-ma-template/) from time to time.
