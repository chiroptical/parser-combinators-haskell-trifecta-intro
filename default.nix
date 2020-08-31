{ packages ? import ./pkgs.nix }:
let
  inherit (packages) pkgs;

  inputs = [
    (pkgs.texlive.combine {
      inherit (pkgs.texlive)
        scheme-basic
        beamer
        etoolbox
        translator
        minted
        fvextra
        fancyvrb
        upquote
        lineno
        catchfile
        xstring
        framed
        float
        helvetic
        mathtools
        listings
        ulem
        booktabs;
    })
    pkgs.python38Packages.pygments
    pkgs.which
  ];
in
pkgs.stdenv.mkDerivation {
  name = "haskell-intro-type-safe-db-libs";
  buildInputs = inputs;
  src = builtins.fetchurl {
    url = "https://github.com/chiroptical/parser-combinators-haskell-trifecta-intro/archive/v0.1.tar.gz";
    sha256 = "0ivd8bzv4ws954iy29gybfq6v2hc1y2ia8b5ki6xqccwrvfi73bv";
  };
  buildPhase = ''
    source $stdenv/setup
    mkdir -p $out
    pdflatex -shell-escape ./parsing.tex
    pdflatex -shell-escape ./parsing.tex
    pdflatex -shell-escape ./parsing.tex
  '';
  dontInstall = true;
}
