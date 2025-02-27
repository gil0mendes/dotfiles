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
