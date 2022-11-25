{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };

  outputs = { self, flake-utils, opam-nix, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system:

      let pkgs = nixpkgs.legacyPackages.${system};
          on = opam-nix.lib.${system};
          scope = on.buildOpamProject { } "ppx_deriving_zipper" ./. {
            merlin = "*";
            ocaml-base-compiler = "*";
            ocaml-lsp-server = "*";
            ocp-indent = "*";
            utop = "*";
          };
      in
        {
          packages = scope // {
            default = self.packages.${system}.ppx_deriving_zipper;
          };

          devShells.default = pkgs.mkShell {
            buildInputs = [
              scope.merlin
              scope.ocaml-lsp-server
              scope.ocp-indent
              scope.utop
            ];
            inputsFrom = [ scope.ppx_deriving_zipper ];
          };
        });
}
