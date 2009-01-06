#!/usr/bin/env bash

echo -n "python: "
echo -n $(python --version)
# printf '%10s' $(python --version)
echo "lisp: "$(sbcl --version)
echo "prolog: " $(swipl --version)
echo "os:" $(uname -sr)
echo "gnuplot: "$(gnuplot --version)
echo "bash: "$(bash --version)
echo "pydot: "$(sudo port info py25-dot | head -n 1)