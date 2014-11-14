# Update xconfig
export PATH=/opt/xconfig:$PATH
xconfig update

# Load modules
for zshrc in ~/.zsh.d/*.zshrc; do
    source $zshrc
done

# Private config
if [ -f ~/.private.zshrc ]; then
    source ~/.private.zshrc
fi
