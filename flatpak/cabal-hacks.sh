#!/bin/bash
# workaround https://github.com/haskell/cabal/issues/8923 by making pkg-config fail
# when given more than one package, instead of silently using only the first package 
mkdir -p pkg-config-hack
cat <<'EOF' > pkg-config-hack/pkg-config
  #!/bin/bash
  if [ "$1" == "--modversion" ] && [ "$2" != "" ] && [ "$3" != "" ]; then
    exit 1
  fi
  exec /usr/bin/pkg-config $@
EOF
chmod +x pkg-config-hack/pkg-config
export PATH=$(pwd)/pkg-config-hack:$PATH