# <img src=".github/logo.png" align="right" width="100"> Inga

[![CI](https://github.com/seachicken/inga/actions/workflows/ci.yml/badge.svg)](https://github.com/seachicken/inga/actions/workflows/ci.yml)

A static analysis tool that searches references from changed code to detect entry points that have a strong impact on the user.

<img width="901" alt="image" src="https://user-images.githubusercontent.com/5178598/201501698-11030115-678b-47ab-a22b-b21e8fc84694.png">

## Why?

Code changes daily, and it is always important to check the impact of changes.
In many cases, checking the impact of a change depends on how vigilant the author is in writing the code, making it difficult to detect unintended effects during the code review and QA phases.
This tool improves software quality by detecting unintended changes at an early phase.

## Supported Languages

- Java
- JavaScript
- Kotlin
- TypeScript

## Usage

```sh
inga [options]
```

### Options

`--base-commit <string>`

Analyze the difference between the --base-commit and the checked-out commit. Set refname or SHA.

`--root-path <string>`

Relative path of the project to be analyzed, so if you do not give this option, it defaults to the command execute path.

`--include <string>`

Filenames of glob pattern matching to include from analysis. (e.g. "core/**/*.ts")

`--exclude <string>`

Filenames of glob pattern matching to exclude from analysis. (e.g. "**/*.test.(ts|tsx)")

### Run on GitHub Actions

[Inga Action](https://github.com/seachicken/inga-action)

#### Example projects

- [spring-boot-realworld-example-app](https://github.com/seachicken/spring-boot-realworld-example-app/blob/master/.github/workflows/inga.yml)
