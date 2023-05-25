#!/bin/bash

curl -s localhost:9083/tip | jq '.tipBlockNo'
