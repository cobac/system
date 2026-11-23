{
  description = "Bitwig Studio 5.2.7 with compatible Mesa/Vulkan libraries, arch btw";

  inputs = {
    # nixpkgs 24.05 has Mesa ~24.1.x, compatible with Bitwig 5.2.7
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      bitwig-studio = pkgs.stdenv.mkDerivation rec {
        pname = "bitwig-studio";
        version = "5.2.7";

        src = pkgs.fetchurl {
          # from aur 5.0 package
          url = "https://www.bitwig.com/dl/Bitwig%20Studio/${version}/installer_linux/";
          name = "${pname}-${version}.deb";
          hash = "sha256-Tyi7qYhTQ5i6fRHhrmz4yHXSdicd4P4iuF9FRKRhkMI=";
        };

        nativeBuildInputs = [ pkgs.dpkg ];

        unpackPhase = ''
          runHook preUnpack
          dpkg-deb -x $src .
          runHook postUnpack
        '';

        dontConfigure = true;
        dontBuild = true;
        dontFixup = true;

        installPhase = ''
          runHook preInstall
          mkdir -p $out
          cp -r opt/bitwig-studio $out/
          runHook postInstall
        '';
      };
    in
    {
      apps.${system}.default = {
        type = "app";
        program = "${self.packages.${system}.bitwig-wrapped}/bin/bitwig-studio-wrapped";
      };

      packages.${system} = {
        inherit bitwig-studio;
        default = self.packages.${system}.bitwig-wrapped;

        bitwig-wrapped = pkgs.writeShellScriptBin "bitwig-studio-wrapped" ''
          export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [
            pkgs.mesa
            pkgs.mesa.drivers
            pkgs.vulkan-loader
            pkgs.libglvnd
            pkgs.xorg.libX11
            pkgs.xorg.libxcb
            pkgs.xorg.libXcursor
            pkgs.xorg.libXrender
            pkgs.xorg.libXfixes
            pkgs.libxkbcommon
            pkgs.xcb-imdkit
            pkgs.xorg.xcbutilwm
          ]}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"

          # Point Vulkan ICD loader to the Nix-provided Intel driver
          export VK_ICD_FILENAMES="${pkgs.mesa.drivers}/share/vulkan/icd.d/intel_icd.x86_64.json"

          exec ${bitwig-studio}/bitwig-studio/bitwig-studio "$@"
        '';
      };

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          pkgs.mesa
          pkgs.mesa.drivers
          pkgs.vulkan-loader
          pkgs.vulkan-tools
          pkgs.libglvnd
        ];

        shellHook = ''
          export VK_ICD_FILENAMES="${pkgs.mesa.drivers}/share/vulkan/icd.d/intel_icd.x86_64.json"
          echo "Mesa version: $(cat ${pkgs.mesa}/nix-support/mesa-version 2>/dev/null || echo 'unknown')"
          echo "Run: /opt/bitwig-studio/bitwig-studio"
        '';
      };
    };
}
