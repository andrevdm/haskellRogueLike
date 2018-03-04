#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueLevels-exe; stack build --fast; stack run &"
