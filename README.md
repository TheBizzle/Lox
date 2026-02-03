Lox
===============

## Based On...

https://craftinginterpreters.com/

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
git clone https://github.com/munificent/craftinginterpreters.git mothership
cd mothership
make get
make
cd ..
```

### Running the tests

```sh
./test.sh
```

## License

Public domain.
