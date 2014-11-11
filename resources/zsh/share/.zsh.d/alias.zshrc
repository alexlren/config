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

alias grep='grep --color'
alias ls='ls --classify --tabsize=0 --literal --color=auto --show-control-chars --human-readable'
alias cp='cp -v'
alias mv='mv -i -v'
alias rm='rm -i -v'
alias ll='ls -l'
alias la='ls -la'
alias l='la'
alias c='clear'
alias ew='__wemacs emacs -nw'
alias e='__wemacs emacsclient --alternate-editor="" -t'
alias en='e'
alias se='sudoedit'
alias less='less --quiet'
alias p='cd ..'
alias df='df --human-readable'
alias du='du --human-readable'
alias m='mutt -y'
alias md='mkdir'
alias rd='rmdir'
alias m='make'
alias clean='find . -regex ".*~" -exec rm -f {} \;'
alias cleanall='sudo find / -regex ".*~" -exec rm -f {} \;'
alias tm='tmux -u'
alias tml='tmux ls'
alias tmk='tmux kill-session -t'
alias tma='tmux -u attach -t'
alias lt='tree'
alias lta='tree -a'
alias urxvt='urxvt256c'
