# Load modules
for zshrc in ~/.zsh.d/*.zshrc; do
    source $zshrc
done

# Node.js
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
export PATH=$PATH:$HOME/.yarn/bin

# Local bins priority
export PATH=$HOME/.local/bin:$PATH

# Android
export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools
export JAVA_HOME=/etc/alternatives/jre_15
export PATH=$JAVA_HOME/bin:$PATH

# Rust
export PATH=$PATH:$HOME/.cargo/bin
export RUST_SRC_PATH=$(rustc --print sysroot)/lib/rustlib/src/rust/library

# Go
export PATH=$PATH:/usr/local/go/bin
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin

if [ -f ~/.zshrc.local ]; then
    source ~/.zshrc.local
fi
