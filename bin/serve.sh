#!/bin/bash

set -euxo pipefail

stack build
stack exec -- site server --host 0.0.0.0
