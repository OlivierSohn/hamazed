FROM imajuscule/serve-highscores

ENTRYPOINT ["/usr/local/bin/imj-serve-highscores-exe"]

# We pass -pPORT to indicate that the port number should be deduced from the "PORT"
# environment variable (this is the way Heroku conveys the information on port number).
# Hence, when testing locally, -pPORT should be overriden by passing -p<portNumber>
# to the command running the container.

CMD ["-pPORT"]
