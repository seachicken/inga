# Contributing

Hi! Thanks for your interest in contributing to Inga.

Please let us know about bugs and feature requests via [GitHub issues](https://github.com/seachicken/inga/issues/new/choose).  
If you find a security vulnerability, do NOT open an issue. Email lavish.hut.0z@icloud.com.

## Building the project

### Prerequisites

- **Roswell**: Install from [roswell](https://github.com/roswell/roswell#installation-dependency--usage).
- **Node v16**

```shell
# Install dependencies for TypeScript
npm install -g typescript
git clone git@github.com:seachicken/tsparser.git
npm install -g </youer/local/path/tsparser>

# Install dependencies for Java
curl -o ./libs/lombok.jar -L https://projectlombok.org/downloads/lombok.jar

# Add environment variables to .zprofile, etc
export INGA_HOME=</your/local/path>/inga

git clone <this repository> --recursive

./roswell/inga.ros <options>
```
