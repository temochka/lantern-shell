#!/bin/bash
set -ea

case $1 in
  "dev")
    esbuild src/index.js --bundle --sourcemap --outfile=dist/public/js.js
    elm make src/LanternShell.elm --output=dist/public/elm.js
    ;;

  *)
    esbuild src/index.js --bundle --minify --outfile=dist/public/js.js
    elm make src/LanternShell.elm --optimize --output=dist/public/elm.js
    ;;
esac
