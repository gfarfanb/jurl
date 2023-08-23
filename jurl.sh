#! /usr/bin/env bash

find . -type f -name 'jurl-*.jar' -exec java -jar {} $* \; >&2
