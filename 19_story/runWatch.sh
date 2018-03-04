#! /bin/sh

find src app test -name '*hs' | entr sh -c "killall rogueStory-exe; stack build --fast; stack run &"
