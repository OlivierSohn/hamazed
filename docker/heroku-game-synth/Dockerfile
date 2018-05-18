# imajuscule/hamazed can be built locally using 'stack image container'

FROM imajuscule/hamazed  

# To run another game, change the name of the executable hereunder:

ENTRYPOINT ["/usr/local/bin/imj-game-synths-exe", "-s"]

# We pass '-l console' to make the server more verbose.
#
# We pass -pPORT to indicate that the port number should be deduced from the "PORT"
# environment variable (this is the way Heroku conveys the information on port number).
# Hence, when testing locally, -pPORT should be overriden by passing -p<portNumber>
# to the command running the container.

CMD ["-l", "console", "-pPORT"]
