#!/bin/bash
find posts -type l -exec rm -vf {} \+
set -e -o pipefail
stack setup
npm install
stack build 
rm -rf out
npm run build
stack exec -- site rebuild
