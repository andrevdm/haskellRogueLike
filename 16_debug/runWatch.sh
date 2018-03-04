#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueDebug-exe; stack build --fast; stack run &"
