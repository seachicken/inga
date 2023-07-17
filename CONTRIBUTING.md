# Contributing

Hi! Thanks for your interest in contributing to Inga.

Please let us know about bugs and feature requests via [GitHub issues](https://github.com/seachicken/inga/issues/new/choose).  
If you find a security vulnerability, do NOT open an issue. Email lavish.hut.0z@icloud.com.

## Running the project

### Prerequisites

- **Roswell**: Install from [roswell](https://github.com/roswell/roswell#installation-dependency--usage).

```shell
git clone <this repository> --recursive

# Add environment variables to .zprofile, etc
export INGA_HOME={inga root path}

{inga root path}/roswell/inga.ros <options>
```

### Prerequisites for Java / Kotlin

```shell
curl -o {inga root path}/libs/javaparser.jar -L https://github.com/seachicken/javaparser/releases/download/javaparser-{version}/javaparser-{version}.jar
curl -o {inga root path}/libs/ktparser.jar -L https://github.com/seachicken/ktparser/releases/download/ktparser-{version}/ktparser-{version}.jar
```

- [javaparser](https://github.com/seachicken/javaparser/releases)
- [ktparser](https://github.com/seachicken/ktparser/releases)

### Prerequisites for JavaScript / TypeScript

- **Node v16**

```shell
npm install -g typescript @seachicken/tsparser
```

- [tsparser](https://github.com/seachicken/tsparser/releases)

### Run

```shell
{inga root path}/roswell/inga.ros <options>
```
