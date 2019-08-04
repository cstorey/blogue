#!/bin/bash

set -euxo pipefail

stack build
stack exec -- slick .venv/bin/python
.venv/bin/python -m http.server --directory _site
