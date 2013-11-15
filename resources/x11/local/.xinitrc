DEFAULT_SESSION=startfluxbox

case $1 in
     fluxbox)
        exec startfluxbox
        ;;
    xfce4)
        exec startxfce4
        ;;
   *)
        exec $DEFAULT_SESSION;;
esac
