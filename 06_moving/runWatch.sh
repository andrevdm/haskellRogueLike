#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueMoving-exe; stack build --fast; stack run &"
