#!/bin/bash
set -e -o pipefail
npm install
stack build 
stack exec -- site rebuild
