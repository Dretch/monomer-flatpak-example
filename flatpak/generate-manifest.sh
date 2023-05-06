#!/bin/bash -ex

WORK_DIR=$(pwd)
VERSION=$(sed -nr 's/^version:\s*(.*)/\1/p' ../package.yaml)

source cabal-hacks.sh

# create a cabal build plan for the published package
(cd /tmp \
 && cabal update \
 && rm -rf monomer-flatpak-example-$VERSION \
 && cabal unpack monomer-flatpak-example-$VERSION \
 && cd monomer-flatpak-example-$VERSION \
 && cabal new-build --dry-run --disable-tests --disable-benchmarks --with-compiler=ghc-9.2.5)

# add packages from the cabal build plan into our template flatpak manifest, using a
# patched cabal-flatpak so that alex, happy and c2hs are included in the manifest
(cd /tmp  \
 && wget https://hub.darcs.net/Dretch/cabal-flatpak/dist -O cabal-flatpak-patched \
 && unzip -o cabal-flatpak-patched \
 && cd cabal-flatpak \
 && cabal build \
 && cabal run cabal-flatpak -- \
  --cabal-install \
  --arch=x86_64 \
  --build-library-exes \
  --cabal-install-serial \
  --directory=/tmp/monomer-flatpak-example-$VERSION \
  $WORK_DIR/io.github.Dretch.MonomerFlatpakExample.template.yml \
  $WORK_DIR/io.github.Dretch.MonomerFlatpakExample.yml)

# insert hack to workaround cabal bug: https://github.com/haskell/cabal/issues/8923
head -n 80 io.github.Dretch.MonomerFlatpakExample.yml > io.github.Dretch.MonomerFlatpakExample.yml.fixed
cat <<'EOF1' >> io.github.Dretch.MonomerFlatpakExample.yml.fixed
  - |
    # workaround https://github.com/haskell/cabal/issues/8923 by making pkg-config fail
    # when given more than one package, instead of silently using only the first package 
    mkdir -p pkg-config-hack
    cat <<'EOF2' > pkg-config-hack/pkg-config
      #!/bin/bash
      if [ "$1" == "--modversion" ] && [ "$2" != "" ] && [ "$3" != "" ]; then
        exit 1
      fi
      exec /usr/bin/pkg-config $@
    EOF2
    chmod +x pkg-config-hack/pkg-config
    export PATH=$(pwd)/pkg-config-hack:$PATH
    cabal --config-file=.cabal/config install -j1 --offline --prefix=/app *.tar.gz
EOF1
tail -n +82 io.github.Dretch.MonomerFlatpakExample.yml >> io.github.Dretch.MonomerFlatpakExample.yml.fixed
mv io.github.Dretch.MonomerFlatpakExample.yml.fixed io.github.Dretch.MonomerFlatpakExample.yml

# insert desktop file installation at the end, because it needs assets from this app's cabal package
cat <<EOF >> io.github.Dretch.MonomerFlatpakExample.yml
- name: install-desktop-files
  buildsystem: simple
  build-commands:
  - mkdir -p /app/share/applications
  - mkdir -p /app/share/icons/hicolor/512x512/apps
  - mkdir -p /app/share/metainfo
  - cp /app/share/*/monomer-flatpak-example-*/*.desktop /app/share/applications
  - cp /app/share/*/monomer-flatpak-example-*/*.png /app/share/icons/hicolor/512x512/apps
  - cp /app/share/*/monomer-flatpak-example-*/*.metainfo.xml /app/share/metainfo
EOF
