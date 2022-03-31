#!/bin/bash
set -eax
brew update
brew install elm
npm install -g elm-live esbuild
npm install
