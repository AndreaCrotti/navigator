#!/usr/bin/env bash
# this works only if you have sbcl installed

sbcl --noinform --load nav.lisp --eval "(navigator)" --eval "(quit)" --noprint --noinform 2>/dev/null