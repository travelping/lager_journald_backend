Overview
--------
This is a backend for [Lager](https://github.com/basho/lager)

It will send logs out of lager to systemd journald.


Configuration
-------------
Configure Lager like this:

  `{handlers, [
      {lager_journald_backend, [{level, info}]}
  ]}`

All logs with the level of the configuration or higher will be send to journald.
The default level is info, so in the case above you can simply write:

  `{handlers, [
      {lager_journald_backend, []}
  ]}`

Dependency
----------
lager_journald_backend uses [ejournald](https://github.com/travelping/ejournald).
