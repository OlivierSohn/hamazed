# What is it?

Haskell bindings to a C++ audio engine, using portaudio underneath to connect to
the audio platform.

The C++ audio engine is designed with performance in mind,
caring about memory locality / alignment to make algorithms run faster.

The convolution reverb algorithm is dynamically adjustable: based on the
speed of computations, it adjusts the way the overall computation is carried
so as to minimize the likelyhood of a missed audio deadline.

Double precision arithmetic, so as to reduce the numeric noise induced when doing
long FFTs.

And it features:

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
  - Zero-latency convolution reverbs. Very long responses can be used
  and the computation scheme uses dynamic optimization to figure out the best
  way to carry the computation, so that every audio callback finishes in time.

  CPU usage (in percentages of a single core) for a 2015 MacBook Air laptop,
    Intel Core i7 / 2,2 GHz:

                                      Using FFTs from:
                                     Accelerate   naive
    2-channels, 17 seconds long   :   12%          30%
    4-channels, 12 seconds long   :   17%          

  Accelerate is available on OSX, so on Linux only shorter room responses can be used
  without underruns. This could be fixed by using an optimized FFT library on linux, too,
  like FFTW or Blas.

  - A compressor limits the audio output to prevent it from clipping.

# What's next ?

- Fix performance on Linux by using the equivalent of Accelerate on OSX (Blas or FFTW)
- The ability to modify the sound characteristics while a note is being played.
- Make more audio engine instruments available:
  some are based on frequency sweeps, to emulate birds singing, others are
  based on markov chains to emulate robotic sounds.
- Investigate using OpenCL for (some FFTs of a) convolution reverbs, using
  fast submission to avoid OpenCL submission latency
  (see https://www.iwocl.org/wp-content/uploads/iwocl-2016-gpu-daemon.pdf).
  We could parallelize early and late coefficients handling.

# Supported platforms

Officially supported client platforms are macOS and Ubuntu.

# Build

The C++ sources use C++17, hence recent enough compilers (`clang`, `gcc`)
are needed to build the package.
