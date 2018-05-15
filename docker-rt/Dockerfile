# This docker file contains runtime dependencies for running game servers made with imj-game.
#
# The reason we have audio and rendering dependencies server-side is because the executable
# can be client, server or both depending on command line arguments. Hence, eventhough no audio
# will be used on the server, and no font rendering will be done at runtime, we need the
# libraries to be installed.

FROM phusion/baseimage

RUN apt-get update && apt-get install -y \
    libstdc++6 \
    libxcursor-dev \
    libxi-dev \
    libxinerama-dev \
    libxrandr-dev \
    libgmp-dev \
    portaudio19-dev \
    libftgl-dev

# to upgrade libstdc++6:
RUN add-apt-repository ppa:ubuntu-toolchain-r/test -y
RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get dist-upgrade -y

