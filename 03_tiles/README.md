# Tiles 

 - html
   - hidden image to load tiles
   - tiles.png route to tileset
 - js
   - drawTile
   - tileFromTileId
   - getCachedBlankCanvas
 - haskell
   - entity types and functions added
   - id = (100 * x + y), means one less value to send
   - blank = id of blank cell, the cell most used. For now that is 41 x 13
   - entity vs tile. Same entity could have different tiles depending on state.
     if you don't want that this can be simplified to only having tiles
   - entity can have a "load text" that is a text string used when loading a world from a csv
   - UiDrawCommad
   - Send a draw command in response to a redraw request
