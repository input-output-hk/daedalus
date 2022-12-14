#!/usr/bin/env bash

if [[ "$CI" != "true" ]]; then
  yarn lockfile:fix
fi
