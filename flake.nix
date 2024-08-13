{
  description = "Callisto is a reverse polish notation programming language inspired by YSL-C3 and Forth";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";

  outputs = { nixpkgs, ... }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "i686-linux" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in
    {
      packages =
        forEachSupportedSystem
          ({ pkgs }:
            let
              cac = pkgs.buildDubPackage rec {
                pname = "cac";
                name = pname;

                src = ./.;
                dubLock = ./dub-lock.json;

                nativeBuildInputs = [ pkgs.makeWrapper ];

                installPhase = ''
                  runHook preInstall
                  install -Dm755 cac -t $out/bin
                  runHook postInstall
                '';

                postFixup = with pkgs; ''
                  wrapProgram $out/bin/cac --prefix PATH : "${lib.makeBinPath [ nasm stdenv.cc uxn ]}"
                '';
              };
            in
            {
              inherit cac;
              default = cac;
            });
      devShells =
        forEachSupportedSystem
          ({ pkgs }: {
            default = pkgs.mkShell {
              packages = with pkgs; [ dmd dub nasm ];
            };
          });
    };
}
