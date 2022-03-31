#!/bin/bash
set -ea

./scripts/build.sh dev
elm-live "src/LanternShell.elm" --dir="dist/public" --pushstate --hot -- --output="dist/public/elm.js"
