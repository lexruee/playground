#!/usr/bin/env bash

if [ -z $(which http-server) ]; then
    echo "Please install node and the module http-server"
    echo "npm install http-server -g"
    echo ""
else
    http-server -p 8000
fi
