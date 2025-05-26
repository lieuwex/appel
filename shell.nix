with (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/release-25.05.tar.gz") {});

pkgs.mkShell {
	buildInputs = [
		rust-bin.stable.latest.complete
		cargo-outdated
		cargo-show-asm
		cargo-flamegraph

		openssl
		pkg-config
		gnum4
	];
}
