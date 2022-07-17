# Inga

[![CI](https://github.com/seachicken/inga/actions/workflows/ci.yml/badge.svg)](https://github.com/seachicken/inga/actions/workflows/ci.yml)

A static analysis tool that analyzes code diffs to find code that has a large impact on users.

## Why?

- When one function is changed, the impact of the change is more than expected
- Find UI components affected by code changes so you can notice early on if there are any unexpected effects

## Supported languages and frameworks

### front-end

- TypeScript
- React

### back-end

- Java
- Spring Boot

## Usage

```sh
inga [options]
```

### Options

`--front-path <string>`

Relative path of the front-end project to be analyzed.

`--back-path <string>`

Relative path of the back-end project to be analyzed.

`--exclude <string>`

Filenames to exclude from the analysis.

`--github-token <string>`

If GitHub token is set, send the analysis report to comments in pull requests. Analyze with diffs of base and head branch of pull requests.

`--sha-a <string>`

`--sha-b <string>`

Analyze with specific sha-a and sha-b diffs. Not to be used with `--github-token` option.

### Run on GitHub Actions

<img width="897" alt="screenshot" src="https://user-images.githubusercontent.com/5178598/175807955-9cda92ae-de65-4ae5-8ac8-98080f4e1c26.png">

[Example projects](https://github.com/seachicken/create-react-app-typescript-todo-example-2022/blob/master/.github/workflows/inga.yml)

## Development

### Prerequisites

- **Roswell**: Install from [roswell](https://github.com/roswell/roswell#installation-dependency--usage).
- **Node v16**

```shell
# Install dependencies
npm install -g typescript
git clone git@github.com:seachicken/tsparser.git
npm install -g </youer/local/path/tsparser>

git clone <this repository> --recursive
```
