# This file was introduced to be able to push the docker image to Heroku.

# The image referenced hereunder can be built locally using the command:
# > stack image container

FROM imajuscule/hamazed

ENTRYPOINT ["/usr/local/bin/imj-game-synths-exe", "-s"]

# Heroku conveys the information on HTTP port number through the PORT env variable.
# To test locally, just override this by passing -p<number>
CMD ["-l", "console", "-pPORT"]

