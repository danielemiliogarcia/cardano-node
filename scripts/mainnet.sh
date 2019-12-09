#!/usr/bin/env bash

set -e

RUNNER=${RUNNER:-cabal new-run --}
TOPOLOGY=${TOPOLOGY:-"configuration/mainnet-topology.json"}

ARGS=(
        --database-path           "./db/"
        --genesis-file            "configuration/mainnet-genesis.json"
        --genesis-hash            "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
        --topology                "${TOPOLOGY}"
        --socket-dir              "./socket/"
        --config                  "./configuration/mainnet.yaml"
        --port                    7776
        +RTS -K2M
)

${RUNNER} exe:cardano-node "${ARGS[@]}"