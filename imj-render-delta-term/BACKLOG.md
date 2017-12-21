
# Backlog

## Delta rendering

- abstract away the fact that it's a terminal renderer: we could pass
  - an IO function that receives a differences
  - a function to say "render what you received" since last call
    - the "clear" / resize would happen here
The goal is to provide an opengl backend

the vector of differences
the use of putChar / putStr, the definition of rendering commands.
The user of the library passes a function that computes the cheapest way

- when building delta, store the last difference (coords + color) : if the next difference
can be drawn with the same colors as the previous difference, see what is more efficient:
  - 2d position change (like today) : (9)
  - 1d position change                (3,4,4) "\ESC[C" "\ESC[2C" "\ESC[3C"
  - spaces are better for gap of 1,2,3
  - tabs ? "\ESCH" sets a tab at the current location. tabs -2 sets a tab very 2 columns,
    max 20 columns (http://pubs.opengroup.org/onlinepubs/007904875/utilities/tabs.html)
    - if tabs are every 1 columns, (tabs -1) it is equivalent to space : better for 1,2,3
    - if tabs are every 2 columns, and if we are on a tab stop, tabs are better for 2,4,6
    - if tabs are every 3 columns, and if we are on a tab stop, tabs are better for 3,6,9
    - if tabs are every 4 columns, and if we are on a tab stop, tabs are better for 4,8,12
   - (n,targetTabStop) = distanceToPrevTabstop(target)
   - (b,myTabStop) = amIOnTabStop -- returns the closes forward tab stop if not on one yet
   - m = distanceInTabs(myTabStop, targetTabStop)
   - total = if myTabStop <= targetTabStop then n + m + b?1:0 else doitwithspaces
   - compare with usage of position commands and take smallest.

- make tests to see the effect of buffer size on screen tearing :
  - alternate '|' with '-'
  - same with random colors
  - same with worst case : background and foreground change all the time,
        same-background colors are far away (to avoid positional optimization)
- create an app to test engine.

- Merge all (color + position) commands to save 2 bytes.
- Another optimization is to chose the type of "position change"
command we send based on the relative location of successive elements: today
we use exclusively the 2 dimensional version (9 bytes on average), but it would
be more efficient to switch to
  - the "go forward (optionally n times)" version (3 to 5 bytes) when the
  previously rendered element was on the same row.
  - the "printStrLn "

- measure if it's faster to buffer the string sent to putStr on our side

- stats to see stdout buffer usage

- test Unicode support (document, provide an example)

- it could be more efficient to have a global contiguous buffer for the string that will be actually written.

## Misc.

- use hspec for testing
