let
 pkgs = import <nixpkgs> {};
 # choose the ocaml version you want to use
 ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_13;
in
pkgs.mkShell {
  # build tools
  nativeBuildInputs = with ocamlPackages; [ ocaml findlib dune_3 ocaml-lsp ];
  # dependencies
  buildInputs = [
    ocamlPackages.core
    ocamlPackages.ppx_jane
    ocamlPackages.ppx_yojson_conv
    ocamlPackages.utop
    ocamlPackages.yojson
    pkgs.ocamlformat
  ];
  # dune utop lib
  # dune build
  # dune exec ./bin/main.exe
  shellHook = ''
    export NVIM_OCAML_LSP=true
  '';
}
