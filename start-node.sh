#!/usr/bin/env bash
#
# A simple script to start cardano-node instance
# Assumption
# User provides cardano network preprod or preview
#
if [ -z "$1"  ]; then
    echo -e "Usage is \n $0 preview \n or \n $0 preprod \n"
    exit 0
fi
NETWORK=$1
NODE_VERSION='8.93'
echo "+++++ NETWORK = $NETWORK +++++"
DIR="./var/$NODE_VERSION/$NETWORK"
CONFIG_DIR="$DIR/node-config"
LOG_FILE="$DIR/cardano-node-$NETWORK.log"
SOCKET_FILE="$DIR/cardano-node-$NETWORK.socket"
##
## function to download the latest node config files
download_configs() {
    mkdir -p "$CONFIG_DIR"
    pushd "$CONFIG_DIR" || exit
    for i in \
        https://book.world.dev.cardano.org/environments/"$NETWORK"/conway-genesis.json \
        https://book.world.dev.cardano.org/environments/"$NETWORK"/alonzo-genesis.json \
        https://book.world.dev.cardano.org/environments/"$NETWORK"/shelley-genesis.json \
        https://book.world.dev.cardano.org/environments/"$NETWORK"/byron-genesis.json \
        https://book.world.dev.cardano.org/environments/"$NETWORK"/topology.json \
        https://book.world.dev.cardano.org/environments/"$NETWORK"/submit-api-config.json \
        https://book.world.dev.cardano.org/environments/"$NETWORK"/db-sync-config.json \
        https://book.world.dev.cardano.org/environments/"$NETWORK"/config-bp.json \
        https://book.world.dev.cardano.org/environments/"$NETWORK"/config.json
    do wget "$i"
    done
    popd || exit
}
## downlod config files only once
mkdir -p "$DIR"/db
[[ -d "$CONFIG_DIR" ]] || download_configs

cardano-node --version > $DIR/cardano-node-version.txt
cardano-cli --version > $DIR/cardano-cli-version.txt
##
## we don't need to use cabal. Main reason for using cabal is to limit the CPU resources.
##
nohup cardano-node run \
    --topology "$CONFIG_DIR"/topology.json \
    --config "$CONFIG_DIR"/config.json \
    --database-path "$DIR"/db/db \
    --host-addr 0.0.0.0 \
    --port 3001 \
    --non-producing-node \
    --socket-path "$SOCKET_FILE" 2>&1 >$LOG_FILE &
