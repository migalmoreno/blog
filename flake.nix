{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
    ordenada.url = "github:migalmoreno/ordenada";
  };
  outputs =
    {
      nixpkgs,
      systems,
      ordenada,
      ...
    }:
    let
      eachSystem =
        f: nixpkgs.lib.genAttrs (import systems) (system: f (import nixpkgs { inherit system; }));
    in
    {
      devShells = eachSystem (pkgs: {
        default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haunt
            guile
            (emacs.pkgs.withPackages (epkgs: epkgs.htmlize))
            ordenada.packages.${system}.default
          ];
          shellHook = ''
            tmpdir=$(mktemp -d)
            cat ${./projects/ordenada.md} ${
              ordenada.packages.${pkgs.system}.default
            }/index.md > $tmpdir/ordenada.md
            ln -sf $tmpdir/ordenada.md ./posts/projects/ordenada.md
            haunt build
          '';
        };
      });
    };
}
