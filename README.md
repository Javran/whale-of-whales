# Whale of Whales - a simple Telegram Bot

First you need a config file written in YAML,
see [sampe config file](./config-sample.yaml) for an example.

Of course for this to work you need a token for bot API,
and make sure privacy mode is disabled
(otherwise bot cannot receive updates from groups)

After proper setups, run it with:

```bash
stack build && stack exec -- wow ./path/to/your/config.yaml
```
