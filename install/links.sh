print_header "Creating symlinks"
linkables=$(find -H "$DOTFILES" -maxdepth 3 -name '*.symlink')
for file in $linkables ; do
  target="$HOME/.$( basename "$file" '.symlink' )"
  if [ -e "$target" ]; then
    echo "~${target#$HOME} already exists... Skipping."
  else
    echo "Creating symlink for $file"
    ln -s "$file" "$target"
  fi
done

print_header "Installing to ~/.config"
if [ ! -d "$HOME/.config" ]; then
  echo "Creating ~/.config"
  mkdir -p "$HOME/.config"
fi

config_files=$( find "$DOTFILES/config" -d 1 2>/dev/null )
for config in $config_files; do
  target="$HOME/.config/$( basename "$config" )"
  if [ -e "$target" ]; then
    echo "~${target#$HOME} already exists... Skipping."
  else
    echo "Creating symlink for $config"
    ln -s "$config" "$target"
  fi
done

print_header "Installing to ~/.emacs.d"
source="$DOTFILES/emacs"
target="$HOME/.emacs.d"
if [ -e "$target" ]; then
  echo "~${target#$HOME} already exists... Skipping."
else
  echo "Creating symlink for $source"
  ln -s "$source" "$target"
fi
