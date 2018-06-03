# The game executable covers both client and server functionalities.
#
# Hence, even though client functionalities are not used when running the server,
# client run-time dependencies are required to be present.

FROM ubuntu:18.04

# Run-time dependencies for all games based on the 'imj-game' engine:

RUN apt-get update -y

# libpq is needed by a dependency of imj-serve-highscores
RUN apt-get install -y \
    libxcursor-dev \
    libxi-dev \
    libxinerama-dev \
    libxrandr-dev \
    libgmp-dev \
    portaudio19-dev \
    libftgl-dev \
    libpq-dev

