#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueMemory-exe; stack build --fast; stack run &"
