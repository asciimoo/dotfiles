# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME=""
# The almighty $PATH
PATH="$HOME/bin:/usr/local/bin:/bin:/usr/bin:/sbin:/usr/sbin:/usr/games:/usr/X11R6/bin"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
#plugins=(git debian compleat mercurial svn lol python pip github gnu-utils pip tmux)
plugins=(git debian python gnu-utils)

source $ZSH/oh-my-zsh.sh
source $HOME/.zsh_theme

# Customize to your needs...
# Set up aliases
bindkey -M vicmd '^R' history-incremental-pattern-search-backward
bindkey -M viins '^R' history-incremental-pattern-search-backward
HISTFILE=~/.zshhistory
HISTSIZE=100000
SAVEHIST=100000
alias bob='espeak -vhu -p 30 -s 145'
alias enbob='espeak -ven -p 30 -s 145'
alias alice='espeak -vhu -p 70 -s 160'
alias mv='nocorrect mv'       # no spelling correction on mv
alias cp='nocorrect cp'       # no spelling correction on cp
alias mkdir='nocorrect mkdir' # no spelling correction on mkdir
alias feh='feh -F'
alias j=jobs
alias pu=pushd
alias po=popd
alias d='dirs -v'
alias h=history
alias grep=egrep
alias l='ls -l'
alias la='ls -la'
#alias hc='herbstclient'
alias less='less -R'
alias fileserver='python -m SimpleHTTPServer'
alias gpg_fuckin_gui=seahorse
alias lisp='rlwrap sbcl --load ~/.sbclrc'

# List only directories and symbolic
# links that point to directories
alias lsd='ls -ld *(-/DN)'

# List only file beginning with "."
alias lsa='ls -ld .*'

# Filename suffixes to ignore during completion
fignore=(.o .c~ .old .pro, .pyc)

setopt hist_ignore_dups

if [[ $ZSH_VERSION_TYPE == 'new' ]]; then
  setopt \
        hist_expire_dups_first \
        hist_ignore_all_dups \
     NO_hist_no_functions \
        hist_save_no_dups \
        inc_append_history \
        list_packed \
        rm_star_wait
fi

if [[ $ZSH_VERSION == 3.0.<6->* || $ZSH_VERSION_TYPE == 'new' ]]; then
  setopt \
        hist_reduce_blanks
fi

svndiff() {
    svn diff "${@}" | colordiff | less -R
}
gitdiff() {
    git diff "${@}" | colordiff | less -R
}

function sshoff() {
    pkill ssh-agent
    rm /tmp/ssh-agent.session
}
function sshon() {
    tempfile=/tmp/ssh-agent.session

    #Check for an existing ssh-agent
    if [ -e $tempfile ]
    then
        echo "Examining old ssh-agent"
        . $tempfile
    fi

    #See if the agent is still working
    ssh-add -l > /dev/null

    #If it's not working yet, just start a new one.
    if [ $? != 0 ]
    then
        echo "Old ssh-agent is dead..creating new agent."

        #Create a new ssh-agent if needed
        ssh-agent -s > $tempfile
        . $tempfile

        #Add the key
        ssh-add
    fi

    #Show the user which keys are being used.
    ssh-add -L
}

#set -o vi
autoload -U edit-command-line
bindkey -v
zle -N edit-command-line
bindkey -M vicmd  v edit-command-line
ZSH_VERSION_TYPE='new'
#trap 'print "$@" >>/home/a/zshlog' DEBUG
#gotogui() { sudo chvt 7 }
#zle -N gotogui
#bindkey '^[`' gotogui


# stefs historian - https://www.ctrlc.hu/~stef/historian.sh
function start_local_hist() {
    touch ".zshhistory"
    fc -AI "$HISTFILE"
    export HISTFILE=$(echo .zshhistory(:A))
}
autoload _start_local_hist

function realpath() {
    echo $1(:A)
}
autoload realpath

function _set_parent_hist() {
    lh=""
    cdir="./"
    while [[ "$(realpath "$cdir")" != "/" ]]; do
        [[ -O $cdir".zshhistory" && -w $cdir".zshhistory" ]] && {
            lh="$(realpath $cdir".zshhistory")"
            break
        }
        cdir="../$cdir"
    done
    [[ "$lh" != "$HISTFILE" ]] && [[ "$lh" != "" ]] && {
        fc -AI "$HISTFILE"
        export HISTFILE="$lh"
        fc -RI "$HISTFILE"
        echo "new histfile $HISTFILE"
    }
}
autoload _set_parent_hist

chpwd_functions=(${chpwd_functions[@]} "_set_parent_hist")

MAIL=/home/a/.mail && export MAIL
export GOROOT=$HOME/t/go
export PATH=$PATH:$GOROOT/bin
export GOPATH=$HOME/w
source "$HOME/w/ali/ali.sh"
