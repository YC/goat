#!/bin/bash

goat=$1
bad=0

if [ ! -f "$goat" ]; then
    echo "Need goat location" 1>&2
    exit 1
fi

for f in "$(dirname "$0")"/stage1-visible/* "$(dirname "$0")"/fortytwo/*; do
    eval "$goat" -p "$f" &> /dev/null

    rv=$?
    if [ $rv -eq 0 ] && [[ "$f" != *"bad"* ]]; then
        echo "$f passed with exit code 0"
    elif [ "$rv" -ne 0 ] && [[ "$f" == *"bad"* ]]; then
        echo "$f passed with exit code $rv"
    else
        echo "*** $f failed"
        bad=1
    fi
done

if [ "$bad" -eq 1 ]; then
    echo "One or more tests failed"
    exit 1
fi
