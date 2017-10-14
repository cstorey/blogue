#!/bin/bash
set -e -o pipefail
stack setup
npm install
stack build 
npm run build
stack exec -- site rebuild
