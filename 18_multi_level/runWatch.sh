#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueMultiLevel-exe; stack build --fast; stack run &"
