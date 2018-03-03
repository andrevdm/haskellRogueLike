#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueEnergy-exe; stack build --fast; stack run &"
