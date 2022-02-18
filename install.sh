#!/usr/bin/env bash

command_exists() {
  type "$1" > /dev/null 2>&1
}

DOTFILES=$HOME/.dotfiles

source ./install/common.sh
source ./install/links.sh

# only perform macOS-specific install
if [ "$(uname)" == "Darwin" ]; then
  echo -e "\\n\\nRunning on macOS"

  print_header "Installing zplug"
  curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
fi

print_header "creating vim directories"
mkdir -p ~/.local/share/nvim/plugged

print_header "Defining ZSH as default shell"
zsh_path="$( command -v zsh )"
if ! grep "$zsh_path" /etc/shells; then
  echo "adding $zsh_path to /etc/shells"
  echo "$zsh_path" | sudo tee -a /etc/shells
fi

if [[ "$SHELL" != "$zsh_path" ]]; then
  chsh -s "$zsh_path"
  echo "default shell changed to $zsh_path"
fi

echo -e "\\nThanks for using my configs. Now, reload the terminal."
echo -e "\\tGil Mendes<gil00mendes@gmail.com>"
echo -e "\\thttps://github.com/gil0mendes/dotfiles"
