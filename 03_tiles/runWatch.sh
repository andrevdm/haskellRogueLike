#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueTiles-exe; stack build --fast; stack run &"
