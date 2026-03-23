#!/usr/bin/env bash

set -euo pipefail

echo "Building Haskell Lox interpreter...."
stack build

LOX=$(stack path --local-install-root)/bin/lox

echo "Running Lox tests..."
(
  cd ./mothership/ || exit 1
  dart tool/bin/test.dart jlox --interpreter "$LOX"
)
