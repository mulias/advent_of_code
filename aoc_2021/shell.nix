{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  # requires unstable repo added at OS level
  unstable = import <unstable> {};
in

mkShell {
  buildInputs = [
    unstable.swiProlog
    unstable.ruby
    nodejs_22
    elixir_1_15
  ];
}
