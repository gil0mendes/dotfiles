# My Personal Nix configs

## Requirements

In order to install this dotfiles you must to already have installed:

- https://brew.sh
- https://nixos.org/download/

## Installation

```bash
# clone this repository into ~/.dotfiles
git clone git@github.com:gil0mendes/dotfiles.git ~/.dotfiles

# install nix pkg manager
curl -L https://nixos.org/nix/install | sh

# close and open a new terminal

# configure nix to use flakes
mkdir -p ~/.config/nix
cat <<EOF > ~/.config/nix/nix.conf
experimental-features = nix-command flakes
EOF

# Until this is addressed https://github.com/LnL7/nix-darwin/issues/149
sudo mv /etc/nix/nix.conf /etc/nix/.nix-darwin.bkp.nix.conf

# Build the configuration
cd ~/.dotfiles
nix build .#darwinConfigurations.g0m.system

# Enable the configuration
./result/sw/bin/darwin-rebuild switch --flake .#g0m
```

For rebuilding the configuration run:

```bash
darwin-rebuild switch --flake .#g0m
```

### Age secrets

I use some secrets for certain programs like OpenCode when running at my company machine, for telemetry purposes, for them we use Age. The seconds can be found at `./home/secrets/` directory.

Command to decrypt a secret:

```sh
age -d -i $identity -o secret.txt opencode-otlp-headers.age
```

Command to encrypt a secret file:

```sh
age -e -i $identity -o opencode-otlp-headers.age secret.txt
```

Here are the instructions to update the OpenCode values when needed:

```sh
set identity "$HOME/Library/Application\ Support/sops/age/keys.txt"
set recipient (age-keygen -y "$identity")

read -s -P 'OPENCODE_OTLP_ENDPOINT: ' endpoint; printf '\n'
set endpoint_tmp (mktemp home/secrets/.opencode-otlp-endpoint.XXXXXX)
if test -n "$endpoint_tmp"; and printf '%s' "$endpoint" | age -r "$recipient" -a -o "$endpoint_tmp"; and mv "$endpoint_tmp" home/secrets/opencode-otlp-endpoint.age
else
    if test -n "$endpoint_tmp"
        rm -f "$endpoint_tmp"
    end
    echo 'Could not encrypt endpoint; existing ciphertext left unchanged.' >&2
end
set -e endpoint

read -s -P 'OPENCODE_OTLP_HEADERS: ' headers; printf '\n'
set headers_tmp (mktemp home/secrets/.opencode-otlp-headers.XXXXXX)
if test -n "$headers_tmp"; and printf '%s' "$headers" | age -r "$recipient" -a -o "$headers_tmp"; and mv "$headers_tmp" home/secrets/opencode-otlp-headers.age
else
    if test -n "$headers_tmp"
        rm -f "$headers_tmp"
    end
    echo 'Could not encrypt headers; existing ciphertext left unchanged.' >&2
end
set -e headers
```

### Extra steps for Kanata

In order to use Kanata we need to manually install the [VirtualHIDDevice](https://github.com/pqrs-org/Karabiner-DriverKit-VirtualHIDDevice) driver.

After install the driver execute this command:

```sh
/Applications/.Karabiner-VirtualHIDDevice-Manager.app/Contents/MacOS/Karabiner-VirtualHIDDevice-Manager activate
```

## Maintenance

### Upgrade Nix

```sh
sudo -i sh -c 'nix-channel --update && nix-env -iA nixpkgs.nix && launchctl remove org.nixos.nix-daemon && launchctl load /Library/LaunchDaemons/org.nixos.nix-daemon.plist'
```

### Update flake inputs

```sh
flakeup
```

## Inspiration

- https://github.com/malob/nixpkgs
- https://gitlab.com/gil0mendes/dotfiles/
