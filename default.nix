let pkgs = import <nixpkgs> {};
in

{ stdenv ? pkgs.stdenv,
  # fetchgit ? pkgs.fetchgit,
  # makeWrapper ? pkgs.makeWrapper,
  zlib ? pkgs.zlib,
  stack ? pkgs.stack }:

let

in
pkgs.haskell.lib.buildStackProject {
  name = "nicorec";
  buildInputs = [ stack zlib ];
  ghc = pkgs.haskell.compiler.ghc802;
  src = ./.;

  configurePhase = ''
    env
    export HOME=$NIX_BUILD_TOP/fake-home
    mkdir -p fake-home
    export STACK_ROOT=$NIX_BUILD_TOP/.stack
    stack config set system-ghc --global true
  '';

  # installPhase = ''
  #   mkdir -p $out/bin
  #   stack --local-bin-path=$out/bin build --copy-bins
  #   cp -R $src $out/src
  # '';
}

