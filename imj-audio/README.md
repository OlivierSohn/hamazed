# What is it?

Haskell bindings to a C++ audio engine, using portaudio underneath to connect to
the audio platform.

The C++ audio engine is designed with performance in mind:
- caring about memory locality / alignment to make algorithms run faster,
- using dynamic optimization where necessary (for example in convolution reverb)

It used double precision arithmetic, so as to drastically reduce the numeric
noise due to floating point approximations.

It features:

- Polyphonic synthesizers:
  - Using oscillators:
    - sine
    - sine with loudness volume compensation, to produce a sound of equal perceived loudness
    on all frequencies
    - triangle
    - saw
    - square
  - Multiple oscillators running at different frequencies (harmonics) can be combined
    to produce a complex tone.
  - AHDSR envelopes are used to shape the amplitude of the sound.
    Attack, Decay and Release interpolations can be customized with multiple
    easing options.
  - Autorelease mode to skip the Sustain phase.
- The count of simultaneously used synthesizers is limited only by the amount of RAM
  that is present on your machine.
- Postprocessing:
  - Zero-latency convolution reverbs are available. Very long responses can be used
  and the computation scheme uses dynamic optimization to figure out the best
  way to carry the computation.
  (TODO provide an example of CPU usage for 15 second long reverb at 44 kHz).
  - A compressor limits the audio output to prevent it from clipping.

# What's next ?

- The ability to modify the sound characteristics while a note is being played.
- Make more audio engine instruments available:
  some are based on frequency sweeps, to emulate birds singing, others are
  based on markov chains to emulate robotic sounds.

# Supported platforms

Officially supported client platforms are macOS and Ubuntu.

# Build

The C++ sources use C++17, hence recent enough compilers (`clang`, `gcc`)
are needed to build the package.
