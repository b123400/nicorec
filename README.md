# nicorec

[![Build Status](https://travis-ci.org/b123400/nicorec.svg?branch=master)](https://travis-ci.org/b123400/nicorec)

## Build

Install [Stack](https://docs.haskellstack.org/en/stable/README/).

```
stack build
```

## Execute

```
NICONICO_USERNAME=username@example.com \
NICONICO_PASSWORD=aaaaa \
OUT_DIR="./out" \
CO_ID="co1234,KRRRR" \
stack exec nicorec-exe
```

`CO_ID` is a comma separated list, which can contains community ids and channel ids
