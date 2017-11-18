# What is it?

A terminal ascii game I write to practice some Haskell. It has 12 levels of increasing difficulty.

In this game, you control a ship in direction, and you can shoot at moving numbers.
During the 5 first seconds, the ship is immune to collisions with numbers, but after that,
if the ship collides with a number it disintegrates.
When the sum of shot numbers is equal to the objective number, the level is completed.

Keyboard Controls:
- ship acceleration : 's' 'e' 'd' 'f'
- laser shots       : 'j' 'i' 'k' 'l'

Version history:
- master (under development) : add randomness to world generation, walls, and fancier animations.
- 1.0 : The world is a square. (Note : ship acceleration was 'w' 'a' 's' 'd' at that time)

# Credits

Rafael Ibraim published https://gist.github.com/ibraimgm/40e307d70feeb4f117cd which is
delta rendering in the console. It avoids to have to clear the console and redraw
everything at each frame, instead only the parts of the console that have changed are
rendered. It removes unwanted flickering effects.
