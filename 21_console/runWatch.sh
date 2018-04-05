#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueConsole-exe; stack build --fast; stack run &"
