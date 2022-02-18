function print_header() {
  echo -e "\\n========================================"
  echo -e "$1"
  echo "========================================"
} 

# TODO: find a better solution that works with NIX
print_header "Installing NVM"
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash
