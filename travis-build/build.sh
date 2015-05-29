#! /bin/bash
set -e

if grep version "build.sbt" | grep -q "\-SNAPSHOT"; then
  sbt publish
else
  sbt 'set version := version.value + "." + System.getenv("TRAVIS_BUILD_NUMBER")' publishSigned
fi
