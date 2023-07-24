#!/usr/bin/env bash

# This little script is the main core of Bell Setup. Based on the platform
# it's running on, and the user provided options, it will prepare the development
# environment accordingly.

# First things first: Copy the bashrc file to get our nice and comfortable prompt.
# TODO: Write the MacOS version of bashrc and copy the one according to the
# platform running the script.

echo "Copying bashrc to $HOME/.bashrc..."
cp -f bashrc $HOME/.bashrc

# Set up our Emacs things. Besides the .emacs file that gets replaced, we will
# be copying on top of a potentially existing Emacs bundled installation, so
# that we don't ruin whatever was potentially already set up/

echo "Copying emacs-files/emacs.d to $HOME/.emacs.d/..."
cp -R -f emacs-files/emacs.d $HOME/.emacs.d

echo "Copying emacs-files/emacs to $HOME/.emacs..."
cp -f emacs-files/emacs $HOME/.emacs

if [ ! -d "$HOME/Documents" ]; then
    echo -e "\nDocuments folder not found. Creating it right now...\n"
    mkdir -p $HOME/Documents
fi

echo "Copying emacs-files/emacs-dark-mode-themes.txt to \
$HOME/Documents/emacs-dark-mode-themes.txt..."

cp -f emacs-files/emacs-dark-mode-themes.txt $HOME/Documents/

echo "Copying emacs-files/emacs-light-mode-themes.txt to \
$HOME/Documents/emacs-light-mode-themes.txt..."

cp -f emacs-files/emacs-light-mode-themes.txt $HOME/Documents/

