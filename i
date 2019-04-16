#!/bin/bash -e

dune build client/client.exe

cp _build/default/client/client.exe ~/bin/todd
