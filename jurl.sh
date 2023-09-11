#! /usr/bin/env bash

JURL=$(find . -type f -name 'jurl-*.jar')

java -cp ${JURL%/*} -jar $JURL $* >&2
