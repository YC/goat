#!/bin/bash

goat=$1
bad=0

if [ ! -f "$goat" ]; then
    echo "Need goat location" 1>&2
    exit 1
fi

for f in "$(dirname "$0")"/stage1-visible/*.gt \
         "$(dirname "$0")"/fortytwo/*.gt \
         "$(dirname "$0")"/new/*.gt \
         "$(dirname "$0")"/stage3-miles/*.gt \
         "$(dirname "$0")"/stage3-visible/*.gt
do
    eval "$goat" -p "$f" &> /dev/null

    rv=$?
    if [ $rv -eq 0 ] && [[ "$f" != *"bad"* ]]; then
        echo "$f passed with exit code 0"
    elif [ "$rv" -ne 0 ] && [[ "$f" == *"bad"* ]]; then
        echo "$f passed with exit code $rv"
    else
        echo "*** $f failed with exit code $rv"
        bad=$((bad + 1))
    fi
done

for f in "$(dirname "$0")"/stage3-peer/*.gt; do
    bin="$f.o"
    if [[ "$f" == *"codya.gt" ]] || [[ "$f" == *"yiyue.gt" ]]; then
        eval "$goat" "$f" -o "$bin" --disable-bounds-check &> /dev/null
    else
        eval "$goat" "$f" -o "$bin" &> /dev/null
    fi

    rv=$?
    if [ $rv -eq 0 ] && [[ "$f" != *".bad."* ]]; then
        echo "$f compiled with exit code 0"
    elif [ "$rv" -ne 0 ] && [[ "$f" == *".bad."* ]]; then
        echo "$f passed with exit code $rv"
        continue
    else
        echo "*** $f failed with exit code $rv"
        bad=$((bad + 1))
    fi

    in="${f/.gt/.in}"
    out="${f/.gt/.out}"
    if [ ! -f "$in" ]; then
        eval timeout 2 "$bin" | diff - "$out"
    else
        cat "$in" | eval timeout 2 "$bin" | diff - "$out"
    fi

    rv=$?
    if [ $rv -eq 0 ]; then
        echo "$f passed diff"
    elif [ $rv -ne 0 ]; then
        echo "*** $f failed diff"
        bad=$((bad + 1))
    fi
done

if [ "$bad" -ne 0 ]; then
    echo "$bad tests failed"
    exit 1
fi
