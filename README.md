# Flatpak Monomer Example

A demonstration of the [Monomer](https://github.com/fjvallarino/monomer) framework running inside [Flatpak](https://flatpak.org/).

Flatpak provides a relatively easy way to deploy apps on Linux, using a standard format that works on any supporting Linux distrubutions. It has runtme sandboxing and a "portals" system for access to the underlying system: this promises to be particularly useful for Monomer, because it provides things like file choosers that Monomer does not have natively.

## Current Status
Not yet working.

### To format the source code

```bash
# This needs at least ormolu 0.5.0.0 to avoid breaking dot-record syntax
ormolu --mode inplace $(find . -name '*.hs')
```