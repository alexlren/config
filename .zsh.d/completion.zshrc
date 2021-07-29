# ---------------------------------------
# COMPLETION
# ---------------------------------------

zstyle ':completion:*' format "%B->%b$lgreen %d"
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' max-errors 3 numeric
zstyle ':completion:*' completer _complete _correct _approximate
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=** r:|=**'
zstyle ':completion:*' max-errors 1m
zstyle ':completion:*' menu select=5
zstyle ':completion:*' original true
zstyle ':completion:*' squeeze-slashes true

zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' completions 1
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose true


autoload -U compinit
compinit
autoload -U zmv
