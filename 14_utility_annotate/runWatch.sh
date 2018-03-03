#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueUtilityAnnotate-exe; stack build --fast; stack run &"
