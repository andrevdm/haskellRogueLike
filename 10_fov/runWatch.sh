#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueFov-exe; stack build --fast; stack run &"
