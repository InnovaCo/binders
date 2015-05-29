#! /bin/bash
set -e

if grep version "build.sbt" | grep -q "\-SNAPSHOT"; then
  sbt publish
else
  openssl aes-256-cbc -k "$key_password" -in ./travis/inn-oss-public.enc -out ./inn-oss-public.asc -d
  openssl aes-256-cbc -k "$key_password" -in ./travis/inn-oss-private.enc -out ./inn-oss-private.asc -d
  sbt 'set version := version.value + "." + System.getenv("TRAVIS_BUILD_NUMBER")' publishSigned
fi
