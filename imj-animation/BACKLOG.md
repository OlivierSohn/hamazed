
# Backlog

## Implementations

- when an animated point touches the world frame, make it change color

## Design

- Today the contract of animation functions is that they should return
a constant number of animated points that are correlated across frames.
We could also make them say "After frame x, since my animation is done
I will return an empty list" : it could allow to stop animations that have
"CanInteract DontInteract" and will continue indefinitely
or to interpret "CanInteract DontInteract" as "CanInteract Interact" for animations that don't guarantee they will end

- generalize chained sequences on mutations
  - try passing a list of functions to the tree's 'treeOnWall' Rebound
