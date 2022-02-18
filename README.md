# My Personal Nix configs

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

## Inspiration

- https://github.com/malob/nixpkgs
- https://gitlab.com/gil0mendes/dotfiles/
