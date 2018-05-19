#!/bin/bash

# This script deploys the docker containers needed to
#
# - host 'imj-game-synths' and 'imj-game-hamazed' multiplayer games
# - persist highscores of 'imj-game-hamazed'
#
# You can edit the script to skip some parts (between the "this part can be removed" comments):
#   - Unless --highScoresServerName parameter is specified, imj-game-hamazed uses
#   the highscore server deployed here : https://imj-highscores.herokuapp.com/highscores
#   Hence, deploying a new highscores server is not necessary.
#
#   Prerequisites:
#   * The OS on which this script runs matches the OS of the base image ./docker/game-runtime/Dockerfile
#   * Heroku command line is installed
#   * The user is logged in to Docker ('docker login')
#   * The user is logged in to Heroku ('heroku login' and 'heroku container:login')
#   * Apps with names matching the names passed in commands argument ('-a <name>') exist in Heroku.
#
# NOTE: The first script execution will be long, because the entire docker images will be uploaded.
# Subsequent uploads however will be faster, because just the layer that changed will be uploaded.
#
# To monitor and verify the deployment status, use:
#
# > heroku logs --tail -a <name>
#
# Common workarounds:
# ------------------
#
# At some point I encountered the following error (it is a silent error, it doesn't fail the script)
# during the 'heroku container:*' commands:
#
#   unauthorized: authentication required
#     ▸    Error: docker push exited with 1
#
# To fix it, I deleted the "registry.heroku.com" key in ~/.docker/config.json, and ran 'heroku container:login' again


set -e
ROOT="$(pwd)"

speak ()
{
    msg=$1
    bar="-----------------------------------------"
    echo ""
    echo $bar
    echo $msg
    echo $bar
    echo ""
}

speak "We create the base runtime image"

docker build -t "imajuscule/imj-game-rt" "./docker/game-runtime/"

speak "Stack compiles binaries and creates the base game-synth image"

stack image container

speak "We create the base serve-highscores image"

SERVE_HS="$(stack exec which imj-serve-highscores-exe)"
rm -rf "./docker/serve-highscores/usr"
mkdir -p "./docker/serve-highscores/usr/local/bin" && cp "$SERVE_HS" "$_"
docker build -t "imajuscule/serve-highscores" "./docker/serve-highscores/"
rm -rf "./docker/serve-highscores/usr"

speak "We create the base game-hamazed image"

HAMAZED="$(stack exec which imj-game-hamazed-exe)"
rm -rf "./docker/game-hamazed/usr"
mkdir -p "./docker/game-hamazed/usr/local/bin" && cp "$HAMAZED" "$_"
docker build -t "imajuscule/game-hamazed" "./docker/game-hamazed/"
rm -rf "./docker/game-hamazed/usr"

# BEGIN -- this part can be removed
speak "We create and push the Heroku serve-highscores image"

cd "docker/heroku-serve-highscores"
heroku container:push web -a imj-highscores
cd "$ROOT"
# END -- this part can be removed

speak "We create and push the Heroku game-synth image"

cd "docker/heroku-game-synth"
heroku container:push web -a imj-game-synth
cd "$ROOT"

speak "We create and push the Heroku game-hamazed image"

cd "docker/heroku-game-hamazed"
heroku container:push web -a imj-game-hamazed
cd "$ROOT"

speak "Done!"
