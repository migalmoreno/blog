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
      devShells = eachSystem (
        pkgs:
        let
          ox-html-stable-ids = (
            pkgs.emacsPackages.trivialBuild {
              pname = "ox-html-stable-ids";
              version = "0.1.1";
              src = pkgs.fetchFromGitHub {
                owner = "jeffkreeftmeijer";
                repo = "ox-html-stable-ids.el";
                rev = "0.1.1";
                hash = "sha256-58GQlri6Hs9MTgCgrwnI+NYGgDgfAghWNv1V02Fgjuo=";
              };
            }
          );
        in
        {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              haunt
              guile
              (emacs.pkgs.withPackages (
                epkgs: with epkgs; [
                  htmlize
                  ox-html-stable-ids
                  nix-mode
                  nginx-mode
                  rainbow-delimiters
                ]
              ))
            ];
            shellHook = ''
              tmpdir=$(mktemp -d)
              ${toString (
                map
                  (name: ''
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
                  '')
                  (map (path: pkgs.lib.removeSuffix ".org" path) (builtins.attrNames (builtins.readDir ./projects)))
              )}
              export HAUNT_ORG_READER_EMACS_DAEMON_NAME="haunt-build"
              emacs --daemon="$HAUNT_ORG_READER_EMACS_DAEMON_NAME" -Q
              cat > $tmpdir/preamble.el<< EOF
              (require 'ox-html-stable-ids)
              (org-html-stable-ids-add)
              (setq org-html-stable-ids t)
              (require 'nix-mode)
              (require 'nginx-mode)
              (require 'rainbow-delimiters)
              (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
              EOF

              export HAUNT_ORG_READER_EMACS_PREAMBLE=$tmpdir/preamble.el
              export HAUNT_ORG_READER_USE_EMACSCLIENT=1
              haunt build
              emacsclient -s "$HAUNT_ORG_READER_EMACS_DAEMON_NAME" -e "(kill-emacs)"
            '';
          };
        }
      );
    };
}
