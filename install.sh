#!/usr/bin/env bash

command_exists() {
  type "$1" > /dev/null 2>&1
}

DOTFILES=$HOME/.dotfiles

source ./install/common.sh
source ./install/links.sh

if ! command_exists zsh; then
  echo "zsh not found. Please install and then re-run installation scripts"
  exit 1
elif ! [[ $SHELL =~ .*zsh.* ]]; then
  echo "Configuring zsh as default shell"
  chsh -s "$(command -v zsh)"
fi

echo -e "\\nThanks for using my configs"
echo -e "\\tGil Mendes<gil00mendes@gmail.com>"
echo -e "\\thttps://github.com/gil0mendes/dotfiles"
