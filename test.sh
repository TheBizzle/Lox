#!/usr/bin/env bash

set -euo pipefail

echo "Building Haskell Lox interpreter...."
stack build

echo "Running Lox tests..."
(
  cd ./mothership/ || exit 1
  dart tool/bin/test.dart jlox --interpreter "../.stack-work/dist/x86_64-linux/ghc-9.10.3/build/lox/lox"
)
