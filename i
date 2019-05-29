#!/bin/bash -e

dune build client/client.exe

mkdir -p ~/bin
cp _build/default/client/client.exe ~/bin/todd
