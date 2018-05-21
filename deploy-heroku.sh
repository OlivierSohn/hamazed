#!/bin/bash

# This script deploys the docker containers needed to:
# - host 'imj-game-synths' and 'imj-game-hamazed' multiplayer games
# - persist highscores of 'imj-game-hamazed'
#   - Unless --highScoresServerName parameter is specified, imj-game-hamazed uses
#     the highscore server deployed here : https://imj-highscores.herokuapp.com/highscores
#     Hence, deploying a new highscores server may not be necessary:
#     removing the lines between "this part can be removed" will skip this part.
#
# Before running this script, please ensure that:
#   * The OS on which this script runs matches the OS of the base image ./docker/runtime-game/Dockerfile
#   * Heroku command line is installed
#   * The user is logged in to Docker ('docker login') and Heroku ('heroku login' and 'heroku container:login')
#   * Heroku apps were created with names matching the names passed in commands argument ('-a <name>')
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


set -e    # makes the script fail if a command fails

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

speak "We create imajuscule/game-rt"

docker build -t "imajuscule/game-rt" "./docker/runtime-game/"

speak "We create imajuscule/serve-highscores-rt"

docker build -t "imajuscule/postgresql-rt" "./docker/runtime-postgresql/"

speak "Stack compiles binaries and creates the base images"

stack image container

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
