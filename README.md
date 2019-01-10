# Whale of Whales - a simple Telegram Bot

These variables need to be present in the enviroment:

- `BOT_TOKEN` bot API token with "bot" being first 3 characters
- `PULL_TIMEOUT` timeout in seconds for the long pulling
- `KICK_TIMEOUT` timeout in seconds for bot to decide kicking a newly added member
- `ERR_FILE` a file path for outputing error messages (if any)
- `STATE_FILE` a file path for storing bot state (don't have to actually present upon startup)
- `WATCHING_GROUPS` a comma separated list of chat ids that this bot should watch
