#!/usr/bin/env bash

# This little script is the main core of Bell Setup. Based on the platform
# it's running on, and the user provided options, it will prepare the development
# environment accordingly.

bashrc=0
emacs=0
neovim=0
tmux=0
vim=0
vimplugins=0

echo ''

while [ $# -gt 0 ]; do
    case $1 in
        --all)
            bashrc=1
            emacs=1
            neovim=1
            tmux=1
            vim=1
            vimplugins=1
            echo -e "Full setup will be performed!\n"
            ;;

        --bashrc)
            bashrc=1
            echo "Bashrc copy was selected."
            ;;

        --emacs)
            emacs=1
            echo "Emacs setup was selected."
            ;;

        --neovim)
            neovim=1
            vim=1
            echo "Neovim setup was selected. This will also include the Vim basics."
            ;;

        --tmux)
            tmux=1
            echo "Tmux setup was selected."
            ;;

        --vim)
            vim=1
            echo "Vim setup was selected."
            ;;

        --vimplugins)
            vimplugins=1
            echo "Vim Plugins setup was selected."

            if [ $vim -ne 0 ]; then
                echo "Warning: Vim plugins was selected without basic Vim. \
                      It will be assumed basic Vim is ready."
            fi
            ;;
    esac
done

# Copy bashrc to the Home folder.
if [ $bashrc -eq 1 ]; then
    cp bashrc $HOME/.bashrc
fi

