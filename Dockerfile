# This file was introduced to be able to push the docker image to Heroku.

# The image referenced hereunder can be built locally using the command:
# > stack image container
FROM imajuscule/hamazed

# To deploy another game, change the name of the executable hereunder:
ENTRYPOINT ["/usr/local/bin/imj-game-synths-exe", "-s"]

# Heroku conveys the information on HTTP port number through the PORT env variable.
# When testing locally, you should override -pPORT and pass -p<portNumber>
CMD ["-l", "console", "-pPORT"]

