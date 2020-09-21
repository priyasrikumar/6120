#!/usr/bin/env bash

for i in *
do
  if test -f "$i" && [ ${i: -5} == ".bril" ]
  then
    echo "${i%.bril}.json"
    bril2json < $i > "${i%.bril}.json"
  fi
done