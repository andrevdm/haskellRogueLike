#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueStructure-exe; stack build --fast; stack run &"
