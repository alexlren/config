#! /bin/sh

# create xterm-24bits terminfo if
if [ ! -f ~/.terminfo/x/xterm-24bits ]; then
    tmpfile=`mktemp -d`/terminfo-24bit.src
    cat <<EOF > $tmpfile
xterm-24bits|xterm with 24-bit direct color mode,
        use=xterm-256color,
        Tc,
        setb24=\E[48;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
        setf24=\E[38;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
EOF
    tic -x -o ~/.terminfo $tmpfile
    rm -f $tmpfile
fi
