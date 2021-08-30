let
  sources = import ./sources.nix {};
  pkgs = sources.pkgs {};
in
(import ./default.nix).shellFor {
  withHoogle = true;
  tools = {
    cabal = "3.2.0.0";
    hlint = "latest";
    haskell-language-server = "latest";
    ghcid = "latest";
      };
  # nativeBuildInputs = [
    #   pkgs.sqlite
    # ];
}
