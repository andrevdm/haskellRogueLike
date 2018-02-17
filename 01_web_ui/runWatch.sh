#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueWebUi-exe; stack build --fast; stack run &"
