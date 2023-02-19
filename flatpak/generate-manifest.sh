#!/bin/bash

set -ex

VERSION=$(sed -nr 's/^version:\s*(.*)/\1/p' ../package.yaml)

# create a cabal build plan for the published package
(cd /tmp \
 && cabal update \
 && rm -rf monomer-flatpak-example-$VERSION \
 && cabal unpack monomer-flatpak-example-$VERSION \
 && cd monomer-flatpak-example-$VERSION \
 && cabal new-build --dry-run --disable-tests --disable-benchmarks --with-compiler=ghc-9.2.5)

# add packages from the cabal build plan into our template flatpak manifest
cabal-flatpak --directory=/tmp/monomer-flatpak-example-$VERSION io.github.Dretch.MonomerFlatpakExample.template.json io.github.Dretch.MonomerFlatpakExample.json

# insert c2hs after language-c, since c2hs needs language-c and nanovg needs c2hs
# insert desktop file installation at the end, because it needs assets from this app
cat io.github.Dretch.MonomerFlatpakExample.json \
 | jq '.modules |= (map(if type == "object" and .name == "language-c" then [., "extra-libraries/c2hs.json"] else . end) | flatten) + ["install-desktop-files.sh"]' \
 > io.github.Dretch.MonomerFlatpakExample.json.fixed
mv io.github.Dretch.MonomerFlatpakExample.json.fixed io.github.Dretch.MonomerFlatpakExample.json
