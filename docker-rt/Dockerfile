# The game executable covers both client and server functionalities.
#
# Hence, even though client functionalities are not used when running the server,
# client run-time dependencies are required to be present.

FROM phusion/baseimage

# Run-time dependencies for all games based on the 'imj-game' engine:

RUN apt-get update && apt-get install -y \
    libstdc++6 \
    libxcursor-dev \
    libxi-dev \
    libxinerama-dev \
    libxrandr-dev \
    libgmp-dev \
    portaudio19-dev \
    libftgl-dev

# We need this to upgrade libstdc++6:

RUN add-apt-repository ppa:ubuntu-toolchain-r/test -y
RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get dist-upgrade -y
