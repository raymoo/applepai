========
applepai
========

To my horror, there were no complete Mahjong libraries for Haskell. This
library is an attempt to correct this.

Mahjong.Tile provides types and functions related to mahjong tiles.

Mahjong.Group provides types and functions relatedto mahjong tiles.

Parse.MultiSet is a utility module I made for parsing mahjong hands. It is
probably inefficient, I wouldn't recommend using it outside of this. Please let
me know if there is already a decent similar library that I can use instead.
I'm pretty sure there is a problem with the Applicative instance too, since
trying to use ''many'' (or ''some'') results in bottom.