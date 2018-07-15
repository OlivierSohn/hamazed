# What is it?

A game where players collaboratively create a piece of music.

Every player connected to the same server will hear the same music, i.e the music
played by other players along with its own. To play notes, you can use a MIDI keyboard
connected to the computer, or use your computer keyboard.

# Controls

## Instrument definition
- Arrows : select and modify the envelope / tone parameters
- F5  : toggles the view between 'envelope' and 'tone'.

## View
- F6  : toggles the envelope representation 'Linear time' and 'Logarithmic time'.

## Loops
- Space : creates a loop using the current recording.
- F1 to F4: superimposes the current recording to the given loop (or creates the loop
  if it doesn't exist yet)
- F10 : resets the current recording.

## Record music
- Notes can be played / recorded, like on a piano keyboard, but using the computer keyboard.

# Development

- Ideas I'd like to implement include:
  - Loops synchronization
  - Loop delete/mute/unmute
  - Change the length / start of a loop after it's created
  - Possibility to quantize the notes of a loop
  - Output the result as a .wav
  - Save / restore a project
  - Make loop mute / unmute a time parameter.
  - Visual feedback of a project execution using tracks.
