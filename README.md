# Flatpak Monomer Example

A demonstration of the [Monomer](https://github.com/fjvallarino/monomer) framework running inside [Flatpak](https://flatpak.org/).

Flatpak provides a relatively easy way to deploy apps on Linux, using a standard format that works on any supporting Linux distrubutions. It has runtme sandboxing and a "portals" system for access to the underlying operating system: this promises to be particularly useful for Monomer, because it provides things like file choosers that Monomer does not have natively.

## Current Status
Not yet working.

## Additional Tools Needed
- Stack and Cabal (standard Haskell build tools).
- [cabal-flatpak](https://hub.darcs.net/thielema/cabal-flatpak).
- [JQ](https://stedolan.github.io/jq/).

## How to generate the Flatpak manifest
See [generate-flatpak.sh](flatpak/generate-flatpak.sh).

## How to build and install the Flatpak manifest locally
```bash
FLATPAK=~/flatpak

# build the flatpak manifest and install into a local repository
flatpak-builder --force-clean --repo=$FLATPAK/repository --state-dir=$FLATPAK/builder/ $FLATPAK/build/io.github.Dretch.MonomerFlatpakExample flatpak/io.github.Dretch.MonomerFlatpakExample.json

# point our local flatpak at our local repository, and install the app from it
flatpak --user remote-add --no-gpg-verify home-repository $FLATPAK/repository
flatpak --user --reinstall install home-repository io.github.Dretch.MonomerFlatpakExample

# finally run the app
flatpak run io.github.Dretch.MonomerFlatpakExample
```

## FAQs
- **Q. How to load fonts/images within the app?**
- **A.** Use [the normal Cabal mechanism](https://neilmitchell.blogspot.com/2008/02/adding-data-files-using-cabal.html) (for example, see how the fonts are loaded in this app).
- **Q. Why do we need [flatpak/modules](flatpak/modules)?**
- **A.** The cabal buld plan does not contain all the dependencies for Monomer, so we need to manually add the ones it does not have: in particular `nanovg` needs `glu`, `glew` and `c2hs` (which itself needs `language-c` which needs `happy` and `alex`).

## Gotchas
- This process requires your application (not just the libraries it depends on) to be published on Hackage - this might not be something you would otherwise bother with. Changes to `cabal-flatpak` could in theory avoid this requirement - since Flatpak could pull directly from Git tags rather than from Hackage.
- Building the Flatpak is really slow. Subsequent builds will use a cache, though.

## Development Guide
### To format the source code
```bash
# This needs at least ormolu 0.5.0.0 to avoid breaking dot-record syntax
ormolu --mode inplace $(find . -name '*.hs')
```