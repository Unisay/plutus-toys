#!/usr/bin/env bash
#
# A simple script to start cardano-node instance in sancho net
# Assumption
# User provides cardano network preprod or preview
#
NETWORK="sancho"
NODE_VERSION='8.11.0-sancho'
echo "+++++ NETWORK = $NETWORK +++++"
DIR="./var/$NODE_VERSION/$NETWORK"
CONFIG_DIR="./cardano-configuration/network/sanchonet"
LOG_FILE="$DIR/cardano-node-$NETWORK.log"
SOCKET_FILE="$DIR/cardano-node-$NETWORK.socket"
##
mkdir -p "$DIR"
[[ -d "$CONFIG_DIR" ]] || download_configs

cardano-node --version > $DIR/cardano-node-version.txt
cardano-cli --version > $DIR/cardano-cli-version.txt
##
## we don't need to use cabal. Main reason for using cabal is to limit the CPU resources.
##
nohup cardano-node run \
    --topology "$CONFIG_DIR"/cardano-node/topology.json \
    --config "$CONFIG_DIR"/cardano-node/config.json \
    --database-path "$DIR"/db \
    --host-addr 0.0.0.0 \
    --port 3001 \
    --non-producing-node \
    --socket-path "$SOCKET_FILE" 2>&1 >$LOG_FILE &
