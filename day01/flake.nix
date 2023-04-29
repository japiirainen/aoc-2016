{
  description = "AOC-2016 day 01 in java";

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux"
    pkgs = import nixpkgs {inherit system;};
  in {
    packages.${system}.default =
      pkgs.stdenv.mkDerivation
      {
        src = ./;
        name = day01;
        buildPhase = ''
          javac Main.java
        '';
        installPhase = ''
          java Main
        '';
      };
  };
}
