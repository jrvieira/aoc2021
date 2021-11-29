#!/bin/bash

aoc () {

   if [ "$1" = "fetch" ]; then
      ./fetch $2 $3
   elif [ "$1" = "test" ]; then
      ghcid --warnings --lint --no-status --clear --no-height-limit --test=":main $2 test" --test-message="
"
   elif [ "$1" = "run" ]; then
      cabal run -v0 -O2 aoc2021 $2
   elif [[ -z $1 || "$1" = "help" ]]; then
      echo -e "  "
      echo -e "  \033[0;33m# fetch daily input\033[0m"
      echo -e "  aoc fetch [{d}] [{y}]"
      echo -e "  "
      echo -e "  \033[0;33m# daemonize daily tests\033[0m"
      echo -e "  aoc test {d}"
      echo -e "  "
      echo -e "  \033[0;33m# compile and run solution\033[0m"
      echo -e "  aoc run {d}"
      echo -e "  "
   else
      echo "error: invalid command \"$1\""
   fi

}
