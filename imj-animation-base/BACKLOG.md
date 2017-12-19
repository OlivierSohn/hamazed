
# Backlog

- Today the contract of pure animation functions is that they should return
a constant number of animation points that are correlated across frames.
We could also make them say "After frame x, since my animation is done
I will return an empty list" : it could allow to stop animations that have
"CollisionReaction Traverse" and will continue indefinitely
or to interpret "CollisionReaction Traverse" as "CollisionReaction ReboundAnd" for animations that don't guarantee they will end

- generalize chained sequences on collisions
  - try passing a list of functions to the tree's 'treeOnWall' Rebound
