(Adapted from https://contribution-guide-org.readthedocs.io)

# Contributing changes

## Licensing of contributed material

- Anything submitted to a project falls under the licensing terms in the repository’s top level LICENSE file.
- In this case ODbl 1.0 (https://opendatacommons.org/licenses/odbl/summary/):
- Data: You may share, create, and adapt as long as you attribute, share alike, and keep open
- Code: TBD 

## Version control branching

- Always make a new branch for your work, no matter how small. This makes it easy for others to take just that one set of changes from your repository, in case you have multiple unrelated changes floating around.
- A corollary: don’t submit unrelated changes in the same branch/pull request! The maintainer shouldn’t have to reject your awesome bugfix because the feature you put in with it needs more review.

# Code formatting

## Style
- Follow the style you see used in the primary repository! Consistency with the rest of the project always trumps other considerations. It doesn’t matter if you have your own style or if the rest of the code breaks with the greater community - just follow along.

## Naming convention
- Important objects should be named with a prefix describing their class
    - Dataframe: `df.name.otherpartofname.anotherpartofname`
    - List: `ls.name`
- See the README.md file for the naming convention of columns within dataframes


## Referencing data and scripts
- No static addresses!!! Use the code snippet to create `dirsGit`, which contains paths to various subfolders. I.e., all locations of stored data and script should be described relative to the main git folder, with a prepended string for the user's local directory (e.g., '/Users/MFlint').

# Documentation isn’t optional

By “documentation” we mean:
- New features should ideally include updates to prose documentation, including useful example code snippets.
- All submissions should have a changelog entry crediting the contributor and/or any individuals instrumental in identifying the problem.