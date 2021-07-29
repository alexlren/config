# ---------------------------------------
# ALIAS
# ---------------------------------------

# Colors for listing
if [ -x /usr/bin/dircolors ]
then
    if [ -r ~/.dir_colors ]
    then
        eval "`dircolors ~/.dir_colors`"
    elif [ -r /etc/dir_colors ]
    then
        eval "`dircolors /etc/dir_colors`"
    else
        eval "`dircolors`"
    fi
fi


# Base
alias cp='cp -v'
alias mv='mv -i -v'
alias rm='rm -i -v'
alias p='cd ..'
alias md='mkdir'
alias rd='rmdir'
alias ipts='sudo iptables -S'
alias c='clear'
alias df='df --human-readable'
alias du='du --human-readable'
alias less='less --quiet'
alias m='make'

# List
alias ls='ls --classify --tabsize=0 --literal --color=auto --show-control-chars --human-readable'
alias ll='ls -l'
alias la='ls -la'
alias l='la'
alias lt='tree'
alias lta='tree -a'

# Grep
alias grep='grep --color=auto'
alias gr='grep -EHIRn'
alias gri='grep -EHIRin'

# Emacs
alias ne='emacs -nw'
alias e='emacsclient --alternate-editor="" -t'
alias en='e'
alias se='sudoedit'
alias clem='find . -regex ".*~" -exec rm -f {} \;'

# Tmux
alias tm='tmux -u'
alias tml='tmux ls'
alias tmk='tmux kill-session -t'
alias tma='tmux -u attach -t'

# Kubectl
alias k='kubectl'
alias kd='kubectl -n development'
alias kdl='kd get pods'
alias kde='kd exec -it'
alias ks='kubectl -n staging'
alias ksl='ks get pods'
alias kse='ks exec -it'
alias kp='kubectl -n production'
alias kpl='kp get pods'
alias kpe='kp exec -it'

# URXVT
alias urxvt='urxvt256c'
