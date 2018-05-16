# The game executable covers client and server functionality. Command line arguments can be passed
# to run just the server, just the client or both (by default).
#
# This implies that when running as server, we need to have the client run-time dependencies.
#
# This Dockerfile adds client runtime dependencies

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

# The version of libstdc++ must match the one with which the executable was built.
# Here, we upgrade libstdc++6:

RUN add-apt-repository ppa:ubuntu-toolchain-r/test -y
RUN apt-get update -y
RUN apt-get upgrade -y
RUN apt-get dist-upgrade -y
