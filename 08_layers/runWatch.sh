#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueLayers-exe; stack build --fast; stack run &"
