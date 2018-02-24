#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueViewPort-exe; stack build --fast; stack run &"
