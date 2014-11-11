# ----------------------------------------
# KEYS
# ----------------------------------------

# Home
bindkey ''    beginning-of-line
# End
bindkey ''    end-of-line
# Del
bindkey ''    delete-char
# Home
bindkey '^[[1~' beginning-of-line
# End
bindkey '^[[4~' end-of-line
# Del
bindkey '[3~' delete-char
# Insert
bindkey '[2~' overwrite-mode
# PageUp
bindkey '[5~' history-search-backward
# PageDown
bindkey '[6~' history-search-forward
# ctrl + <-
bindkey '[1;5D' backward-word
# ctrl + ->
bindkey '[1;5C' forward-word

typeset -A key

key[Home]=${terminfo[khome]}
key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}

[[ -n "${key[Home]}"    ]]  && bindkey  "${key[Home]}"    beginning-of-line
[[ -n "${key[End]}"     ]]  && bindkey  "${key[End]}"     end-of-line
[[ -n "${key[Insert]}"  ]]  && bindkey  "${key[Insert]}"  overwrite-mode
[[ -n "${key[Delete]}"  ]]  && bindkey  "${key[Delete]}"  delete-char
[[ -n "${key[Up]}"      ]]  && bindkey  "${key[Up]}"      up-line-or-history
[[ -n "${key[Down]}"    ]]  && bindkey  "${key[Down]}"    down-line-or-history
[[ -n "${key[Left]}"    ]]  && bindkey  "${key[Left]}"    backward-char
[[ -n "${key[Right]}"   ]]  && bindkey  "${key[Right]}"   forward-char

