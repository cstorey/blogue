#!/bin/bash

set -euxo pipefail

stack build
stack exec -- slick -- "$@"
