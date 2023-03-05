#!/bin/bash

set -ex

WORK_DIR=$(pwd)
VERSION=$(sed -nr 's/^version:\s*(.*)/\1/p' ../package.yaml)

# create a cabal build plan for the published package
(cd /tmp \
 && cabal update \
 && rm -rf monomer-flatpak-example-$VERSION \
 && cabal unpack monomer-flatpak-example-$VERSION \
 && cd monomer-flatpak-example-$VERSION \
 && cabal new-build --dry-run --disable-tests --disable-benchmarks --with-compiler=ghc-9.2.5)

# add packages from the cabal build plan into our template flatpak manifest, using a
# patched cabal-flatpak so that alex, happy and c2hs are included in the manifest, and
# so the fields in the manifest JSON are in a readable order.
(cd /tmp  \
 && wget https://hub.darcs.net/Dretch/cabal-flatpak/dist -O cabal-flatpak-patched \
 && unzip -o cabal-flatpak-patched \
 && cd cabal-flatpak \
 && cabal build \
 && cabal run cabal-flatpak -- \
  --cabal-install \
  --arch=x86_64 \
  --build-library-exes \
  --directory=/tmp/monomer-flatpak-example-$VERSION \
  $WORK_DIR/io.github.Dretch.MonomerFlatpakExample.template.json \
  $WORK_DIR/io.github.Dretch.MonomerFlatpakExample.json)

# insert desktop file installation at the end, because it needs assets from this app's cabal package
cat io.github.Dretch.MonomerFlatpakExample.json \
 | jq --slurpfile idf install-desktop-files.json '.modules |= . + [$idf[0]]' \
 > io.github.Dretch.MonomerFlatpakExample.json.fixed
mv io.github.Dretch.MonomerFlatpakExample.json.fixed io.github.Dretch.MonomerFlatpakExample.json
