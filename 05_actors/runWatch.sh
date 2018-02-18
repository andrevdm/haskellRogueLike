#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueActors-exe; stack build --fast; stack run &"
