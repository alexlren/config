#######################################
# Zsh xconfig                         #
# Made by shaoner <shaoner@gmail.com> #
#######################################

# -------
# HISTORY
# -------

# History lines
export HISTORY=50000
export SAVEHIST=$HISTORY
export HISTSIZE=$HISTORY

# Path to history file
export HISTFILE=$HOME/.history

# Append to history
setopt append_history

# Each command is added to history
setopt inc_append_history

# Don't add double commands
setopt hist_ignore_dups

# Delete repetitions
#setopt hist_ignore_all_dups

# Delete repetitions when history file is full
setopt hist_expire_dups_first

# Don't save a line more than once
#setopt hist_save_no_dups

# Search don't show double commands
setopt hist_find_no_dups
