#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueStickyLight-exe; stack build --fast; stack run &"
