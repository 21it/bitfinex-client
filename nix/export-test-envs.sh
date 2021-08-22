#!/bin/sh

#
# app
#

export BITFINEX_CLIENT_LOG_ENV="dev"
export BITFINEX_CLIENT_LOG_FORMAT="Bracket" # Bracket | JSON
export BITFINEX_CLIENT_LOG_VERBOSITY="V3"
export BITFINEX_CLIENT_LIBPQ_CONN_STR="postgresql://nixbld1@localhost/bitfinex-client"
export BITFINEX_CLIENT_ENDPOINT_PORT="3000"
