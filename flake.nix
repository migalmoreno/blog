{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
    ordenada.url = "github:migalmoreno/ordenada";
    tubo.url = "github:migalmoreno/tubo";
    nx-router.url = "github:migalmoreno/nx-router";
    nx-tailor.url = "github:migalmoreno/nx-tailor";
    nx-mosaic.url = "github:migalmoreno/nx-mosaic";
    fdroid-el.url = "github:migalmoreno/fdroid.el";
    nyxt-el.url = "github:migalmoreno/nyxt.el";
  };
  outputs =
    inputs@{ nixpkgs, systems, ... }:
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
          ];
          shellHook =
            let
              projects = [
                "ordenada"
                "tubo"
                "nx-router"
                "nx-tailor"
                "nx-mosaic"
                "fdroid-el"
                "nyxt-el"
              ];
            in
            ''
              tmpdir=$(mktemp -d)
              ${toString (
                map (name: ''
                  cat ${./projects/${name}.org} ${
                    if
                      pkgs.lib.hasAttrByPath [
                        "packages"
                        pkgs.system
                        "docs"
                      ] inputs.${name}
                    then
                      "${inputs.${name}.packages.${pkgs.system}.docs}/index.org"
                    else
                      "${inputs.${name}}/README"
                  } > $tmpdir/${name}.org
                  ln -sf $tmpdir/${name}.org ./posts/projects/${name}.org
                '') projects
              )}
              haunt build
            '';
        };
      });
    };
}
