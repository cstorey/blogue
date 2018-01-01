#!/bin/bash
set -e -o pipefail
stack setup
npm install
stack build 
rm -rf out
npm run build
stack exec -- site rebuild
