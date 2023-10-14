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
 && cabal new-build --dry-run --disable-tests --disable-benchmarks)

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
  --directory=/tmp/monomer-flatpak-example-$VERSION \
  $WORK_DIR/io.github.Dretch.MonomerFlatpakExample.template.yml \
  $WORK_DIR/io.github.Dretch.MonomerFlatpakExample.yml)

# insert hack to workaround cabal bug: https://github.com/haskell/cabal/issues/8923
head -n 82 io.github.Dretch.MonomerFlatpakExample.yml > io.github.Dretch.MonomerFlatpakExample.yml.fixed
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
    cabal --config-file=.cabal/config install -j$FLATPAK_BUILDER_N_JOBS --install-method=symlink --installdir=/app/bin monomer-flatpak-example
    find /app/cabal/ghc-* -mindepth 1 -not -path '*/monomer-flatpak-example*' -a -not -path '*/incoming' -delete
EOF1
tail -n +87 io.github.Dretch.MonomerFlatpakExample.yml >> io.github.Dretch.MonomerFlatpakExample.yml.fixed
mv io.github.Dretch.MonomerFlatpakExample.yml.fixed io.github.Dretch.MonomerFlatpakExample.yml

# insert desktop file installation at the end, because it needs assets from this app's cabal package
cat <<EOF >> io.github.Dretch.MonomerFlatpakExample.yml
- name: install-desktop-files
  buildsystem: simple
  build-commands:
  - mkdir -p /app/share/{applications,icons/hicolor/512x512/apps,metainfo}
  - cp /app/cabal/ghc-*/monomer-flatpak-example-*/share/*.desktop /app/share/applications
  - cp /app/cabal/ghc-*/monomer-flatpak-example-*/share/*.png /app/share/icons/hicolor/512x512/apps
  - cp /app/cabal/ghc-*/monomer-flatpak-example-*/share/*.metainfo.xml /app/share/metainfo
EOF
