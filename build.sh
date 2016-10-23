#!/bin/bash
set -e -o pipefail
npm install
stack setup
stack build 
stack exec -- site rebuild
