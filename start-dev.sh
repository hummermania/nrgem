#!/bin/sh
cd `dirname $0`

# NOTE: mustache templates need \ because they are not awesome.

#-config $PWD/energy
ERL_LIBS=deps exec erl -pa $PWD/ebin edit $PWD/deps/*/ebin -sname nrgem -s nrgem_app start \
         -eval "io:format(\"Point your browser at http://localhost:1080~n\")."


