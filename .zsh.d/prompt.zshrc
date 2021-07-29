# ---------------------------------------
# PROMPT
# ---------------------------------------

autoload -U colors && colors
white=%F{015}
green=%F{040}
pink=%F{171}
blue=%F{033}
red=%F{160}
gold=%F{214}

color_user=$blue
color_separator=$white
color_root=$red
color_path=$green
color_branch=$gold
color_ps2=$blue

# Load version control information
autoload -Uz vcs_info

# Format the vcs_info_msg_0_ variable
zstyle ':vcs_info:git:*' formats '%b'
# Set up the prompt (with git branch name)
setopt PROMPT_SUBST

# Display prompt
# %n user
# %m machine
# %(!.#.$) prompt prefix
# %~ current dir
# ${vcs_info_msg_0_} current git branch
# %? exit code of the last command

rprompt="%B[%b$color_path%~%f%B]%b"

precmd() {
    vcs_info
    if [ ! -z "${vcs_info_msg_0_}" ]; then
        export RPS1="$rprompt%B[%b$color_branch${vcs_info_msg_0_}%f%B]%b"
    else
        export RPS1="$rprompt"
    fi
}

export PS1="%(!.$color_root.$color_user)%n${color_separator}@%f%(!.$color_root.$color_user)%m$color_separator:%(0?,%f,$color_error)%?%f%B#%b "
export RPS1="$rprompt"
export PS2="$color_ps2>%f "
