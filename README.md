# Advent of Code 2021

### fetch daily input

./aoc {{1-25}} {{2015-2021}}

### dev monitor

ghcid --warnings --lint --no-status --test-message="" --clear --no-height-limit --test=":main {{01-25}} test"

### run tests + solve

cabal run -v0 -O2 aoc2021 {{01-25}}
