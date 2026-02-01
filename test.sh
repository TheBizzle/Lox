#!/usr/bin/env bash

set -euo pipefail

if ! command -v python3 &>/dev/null; then
  echo "Python3 is required but not found.  Aborting...."
  exit 1
fi

if ! command -v pip &>/dev/null; then
  echo "pip is required but not found.  Aborting...."
  exit 1
fi

if ! command -v loxtest &>/dev/null; then
  echo "Installing loxygen...."
  pip install --user loxygen
fi

if [ ! -d "lox-suite" ]; then
  echo "Setting up Lox test suite...."
  loxtest setup lox-suite
fi

echo "Building Haskell Lox interpreter...."
stack build

echo "Running Lox tests..."
loxtest run --interpreter "stack exec -- lox" "lox-suite"
