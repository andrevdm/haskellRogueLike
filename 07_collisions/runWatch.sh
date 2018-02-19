#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueCollisions-exe; stack build --fast; stack run &"
