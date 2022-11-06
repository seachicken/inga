# Inga

[![CI](https://github.com/seachicken/inga/actions/workflows/ci.yml/badge.svg)](https://github.com/seachicken/inga/actions/workflows/ci.yml)
[![Stake to support us](https://badge.devprotocol.xyz/0xdCF85312C0a1F3f0159aF728286B017739474b07/descriptive)](https://stakes.social/0xdCF85312C0a1F3f0159aF728286B017739474b07)

A static analysis tool that searches references from changed code to detect entry points that have a strong impact on the user.

<img width="902" alt="image" src="https://user-images.githubusercontent.com/5178598/200149748-efc00608-287e-48a3-86c6-22f040b32e92.png">

## Why?

Code changes daily, and it is always important to check the impact of changes.
In many cases, checking the impact of a change depends on how vigilant the author is in writing the code, making it difficult to detect unintended effects during the code review and QA phases.
This tool improves software quality by detecting unintended changes at an early phase.

## Supported languages

- Java
- TypeScript

## Usage

```sh
inga [options]
```

### Options

`--root-path <string>`

Relative path of the project to be analyzed, so if you do not give this option, it defaults to the command execute path.

`--exclude <string>`

Filenames to exclude from the analysis.

`--github-token <string>`

If GitHub token is set, send the analysis report to comments in pull requests. Analyze with diffs of base and head branch of pull requests.

`--base-sha <string>`

Analyze by the difference between the `base-sha` and the checked out sha. Not to be used with `--github-token` option.

`--min-combination <number>`

Minimum number of combinations to display in the results.

### Run on GitHub Actions

#### Example projects

- [create-react-app-typescript-todo-example-2022](https://github.com/seachicken/create-react-app-typescript-todo-example-2022/blob/master/.github/workflows/inga.yml)
- [spring-boot-realworld-example-app](https://github.com/seachicken/spring-boot-realworld-example-app/blob/master/.github/workflows/inga.yml)
- [nestjs-realworld-example-app](https://github.com/seachicken/nestjs-realworld-example-app/blob/prisma/.github/workflows/inga.yml)
