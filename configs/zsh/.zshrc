export DOTFILES=$HOME/.dotfiles
export CACHEDIR="$HOME/.local/share"
export ZSH=$DOTFILES/configs/zsh

[[ -d "$CACHEDIR" ]] || mkdir -p "$CAHCEDIR"

fpath=(
    $DOTFILES/zsh/functions
    /usr/local/share/zsh/site-functions
    $fpath
)

typeset -aU path

export EDITOR='nvim'
export GIT_EDITOR='nvim'

# Load all config files
if [[ -d $DOTFILES/zsh/functions ]]; then
  for func in $DOTFILES/zsh/functions/*(:t); autoload -U $func
fi

for file ($ZSH/lib/*.zsh); do
  source $file
done

########################################################
# Configuration
########################################################

if [ -z "$TMUX" ]; then
  export TERM=xterm-256color-italic
else
  export TERM=tmux-256color-italic
fi

# initialize autocomplete
autoload -U compaudit compinit add-zsh-hook
compinit

# Prepend directories to the global PATH var
prepend_path /usr/local/opt/grep/libexec/gnubin
prepend_path /usr/local/sbin
prepend_path $DOTFILES/bin
prepend_path $HOME/bin
prepend_path $HOME/.emacs.d/bin

# display how long all tasks over 10 seconds take
export REPORTTIME=10

setopt NO_BG_NICE
setopt NO_HUP
setopt NO_LIST_BEEP
setopt LOCAL_OPTIONS
setopt LOCAL_TRAPS
setopt PROMPT_SUBST

setopt COMPLETE_ALIASES

########################################################
# Plugins
########################################################

source ~/.zplug/init.zsh

# oh-my-zsh plugins
zplug "plugins/rust", from:oh-my-zsh
zplug "plugins/brew", from:oh-my-zsh
zplug "plugins/docker", from:oh-my-zsh
zplug "plugins/docker-compose", from:oh-my-zsh
zplug "plugins/git", from:oh-my-zsh
zplug "plugins/sudo", from:oh-my-zsh
zplug "plguins/tmux", from:oh-my-zsh

# Syntax highlighting
zplug "zsh-users/zsh-syntax-highlighting", defer:2

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

[[ -e ~/.terminfo ]] && export TERMINFO_DIRS=~/.terminfo:/usr/share/terminfo

########################################################
# Setup
########################################################

if [[ -a ~/.localrc ]]; then
    source ~/.localrc
fi

[ -f $HOME/.fzf.zsh ] && source $HOME/.fzf.zsh
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# Then, source plugins and add commands to $PATH
zplug load

########################################################
# Misc
########################################################

function sleep-in() {
  local minutes=$1
  local datetime=`date -v+${minutes}M +"%m/%d/%y %H:%M:%S"`
  sudo pmset schedule sleep "$datetime"
}

# Load NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

########################################################
# Aliases
########################################################

# reload zsh config
alias reload!='RELOAD=1 source ~/.zshrc'

# Detect which `ls` flavor is in use
if ls --color > /dev/null 2>&1; then # GNU `ls`
    colorflag="--color"
else # macOS `ls`
    colorflag="-G"
fi

# use nvim, but don't make me think about it
alias vim="nvim"

alias du="ncdu --color dark -rr -x --exclude .git --exclude node_modules"
alias help="tldr"

# tmux aliases
alias ta='tmux attach'
alias tls='tmux ls'
alias tat='tmux attach -t'
alias tns='tmux new-session -s'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# start starship
eval "$(starship init zsh)"


test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
