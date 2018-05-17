#!/bin/bash

# This script deploys a game server to Heroku.

#   Prerequisites:
#   * Heroku command line is installed
#   * The user is logged in (using the command 'heroku login')
#   * An app named "imj-game-synth" exists in Heroku
#   * The stack.yaml contains instructions on how to build the base docker image 

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

speak "Create the base docker image"
stack image container

speak "Create and push the Heroku image"
cd docker-heroku
heroku container:push web -a imj-game-synth

speak "Done!"

