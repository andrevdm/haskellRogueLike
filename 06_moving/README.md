# Moving

 - html
 - js
 - haskell
   - Adding keys, no JS changes!
   - Adding RogueActions type for future use.
     - Not all actions result in a delta (x,y) so handleKey returns an action to be run
     - Multiple actions per key possible, e.g. moving moves + changes some state
   - problems
     - no collision detection
     - completely hides entity standing on
     - moves off screen


### Navigation keys

[http://www.roguebasin.com/index.php?title=Preferred_Key_Controls](Preferred Key Controls)

```
 y k u     home ^ pgup
  \|/          \|/
 h-+-l        <-+->
  /|\          /|\
 b j n      end v pgdn

vi-keys       numpad
```
