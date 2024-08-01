{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  unstable = import <unstable> {};
in

mkShell {
  buildInputs = [
    unstable.ruby_3_1
    unstable.rufo
  ];
  shellHook = ''
    export NVIM_RUFO_LSP=false
  '';
}
