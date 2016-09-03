#! /bin/bash
set -e

if [[ "$TRAVIS_PULL_REQUEST" == "false" && "$TRAVIS_BRANCH" == "master" ]]; then
openssl aes-256-cbc -k "$key_password" -in ./travis/inn-oss-public.enc -out ./inn-oss-public.asc -d
openssl aes-256-cbc -k "$key_password" -in ./travis/inn-oss-private.enc -out ./inn-oss-private.asc -d
  if grep "version\s*:=.*SNAPSHOT" build.sbt; then
    sbt +test +publishSigned
  else
    #sbt 'set version := version.value + "." + System.getenv("TRAVIS_BUILD_NUMBER")' +test +publishSigned sonatypeReleaseAll
  fi
else
  sbt +test
fi
