#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueLoadMap-exe; stack build --fast; stack run &"
