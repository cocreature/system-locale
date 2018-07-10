{ pkgs ? import <nixpkgs> {}
}:
let
  system-locale-src = pkgs.lib.cleanSourceWith {
    filter = path: type:
      let base = baseNameOf path;
      in !(pkgs.lib.hasPrefix ".ghc.environment." base) &&
         !(pkgs.lib.hasSuffix ".nix" base);
    src = pkgs.lib.cleanSource ./.;
  };
  haskellPkgs = pkgs.haskell.packages.ghc843.override(oldAttrs: {
    overrides = self: super: {
      system-locale = super.callCabal2nix "system-locale" system-locale-src {};
    };
  });
in
{ system-locale = haskellPkgs.system-locale;
}
