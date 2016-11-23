#!/bin/bash
set -e -o pipefail
stack setup
stack build 
stack exec -- site rebuild
