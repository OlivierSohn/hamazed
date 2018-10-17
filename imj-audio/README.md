# What is it?

Haskell bindings to a C++ lockfree audio engine, using portaudio underneath to connect to
the audio platform.

# Design goals

## audio callback deadlines

To minimize the likelyhood of a missed audio deadline,
we use no lock, and we make sure that our data is properly placed in memory
to benefit from data locality and to minimize cache misses.

When appropriate, we use auto-adjusting algorithms that probe the run-time conditions
to chose the best (fastest) way of performing a computation.
For example, the 0-latency convolution reverb adjusts its parameters to the actual hardware,
and to the length of the audio callback, so as to minimize
the estimated worst case cost per audio callback.

## Numeric noise

We chose to run all computations in double precision, so as to
minimize the numeric noise induced when doing long FFTs.

## Features:

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
  - Zero-latency, single thread convolution reverbs. Very long responses can be used
  and the computation scheme uses dynamic optimization to figure out the best
  way to carry the computation, so that every audio callback finishes in time.

  No response compression occurs, so responses are used at their full resolution,
  even for the tail of very long responses.

  CPU usage (in percentages of a single core) for a 2015 MacBook Air laptop,
    Intel Core i7 / 2,2 GHz:

                                      using FFTs from:
                                     Accelerate   imj-fft (naive)
    2-channels, 17 seconds long   :   12%          30%
    4-channels, 12 seconds long   :   17%          45%, with buffer underruns.

  Accelerate is available on OSX, so on Linux only shorter room responses can be used
  without underruns. This could be fixed by using an optimized FFT library on linux, too.

  - A compressor limits the audio output to prevent it from clipping.

# What's next ?

- The ability to modify the sound characteristics while a note is being played.
- Make more audio engine instruments available:
  some are based on frequency sweeps, to emulate birds singing, others are
  based on markov chains to emulate robotic sounds.
- Better convolution performance on Linux by using the equivalent of Accelerate on OSX (Blas or FFTW)
- Investigate using OpenCL for (some FFTs of a) convolution reverbs, using
  fast submission to avoid OpenCL submission latency
  (see https://www.iwocl.org/wp-content/uploads/iwocl-2016-gpu-daemon.pdf).
  We could parallelize early and late coefficients handling.
- Lower the room response tail resolution to reduce CPU usage (only when there
  is not enough CPU available)

# Supported platforms

Officially supported client platforms are macOS and Ubuntu.

# Build

The C++ sources use C++17, hence recent enough compilers (`clang`, `gcc`)
are needed to build the package.
