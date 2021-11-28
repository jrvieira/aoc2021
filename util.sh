#!/bin/bash

aoc () {

   if [ "$1" == "fetch" ]; then
      ./fetch $2 $3
   elif [ "$1" == "test" ]; then
      ghcid --warnings --lint --no-status --test-message="" --clear --no-height-limit --test=":main $2 test"
   elif [ "$1" == "run" ]; then
      cabal run -v0 -O2 aoc2021 $2
   else
      echo "error: invalid command"
   fi

}
