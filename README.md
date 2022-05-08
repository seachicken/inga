# Inga

[![CI](https://github.com/seachicken/inga/actions/workflows/ci.yml/badge.svg)](https://github.com/seachicken/inga/actions/workflows/ci.yml)

A static analysis tool of UI components affected by changed code.

## Why?

- When one function is changed, the code is common and the impact can be wider than expected
- Analyzing code diffs and finding the affected UI components can improve the quality of testing for QA and developers

## Usage

### Run on GitHub Actions

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
