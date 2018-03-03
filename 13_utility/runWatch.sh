#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueUtility-exe; stack build --fast; stack run &"
