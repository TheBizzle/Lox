Lox
===============

## Based On...

https://craftinginterpreters.com/

This is a Haskell-based implementation of `jlox` (from the first half of the book, where you build an interpreter in Java), and is fully compliant with the `jlox` suite of tests.

[![Lox CI](https://github.com/TheBizzle/Lox/actions/workflows/lox.yaml/badge.svg)](https://github.com/TheBizzle/Lox/actions/workflows/lox.yaml)

## Testing

### Dart

Install Dart 2.x:

```sh
wget https://storage.googleapis.com/dart-archive/channels/stable/release/2.19.6/sdk/dartsdk-linux-x64-release.zip
unzip dartsdk-linux-x64-release.zip -d $HOME/Applications/dart-2.19.6
export PATH="$HOME/Applications/dart-2.19.6/dart-sdk/bin:$PATH"
```

Make sure to add that last line to your `.zshrc` (or equivalent), for permanent use.

### Building the tester

```sh
git submodule update --init
cd mothership
make get
cd ..
```

### Running the tests

```sh
./test.sh
```

## License

Public domain.
