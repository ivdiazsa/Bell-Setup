#!/usr/bin/env bash

# This little script is to find and clone the repos with my currently installed
# vim 8.0+ plugins. The reason for doing it this way is because of how Git treats
# nested cloned repos, and Git submodules just haven't done the trick. So, we're
# better off fetching them with a script :)

# Helper Functions!

# Check for a directory and create it if it's not there.
# Arguments:
#   1) Directory Path
#   2) Directory name for logging messages

check_for_dir_and_create ()
{
    local dir_path=$1
    local dir_desc=$2

    if [ ! -d $dir_path ];
    then
        echo "$dir_desc path not found. Creating it now..."
        mkdir -p $dir_path
    else
        echo "$dir_desc found..."
    fi
}

# Check for the plugin's repo, and clone it if it's not there.
# Arguments:
#   1) Path to clone the plugin to
#   2) Repo's URL
#   3) Plugin name for logging messages

check_for_repo_and_clone ()
{
    local repo_url=$2
    local repo_desc=$3
    local repo_dest_path=$1/$(basename $repo_desc .git)

    if [ ! -d $repo_dest_path ];
    then
        echo -e "\nFetching $repo_desc to $repo_dest_path...\n"
        git clone $repo_url
    else
        echo -e "\n$repo_desc found...\n"
    fi
}

# The Script!

# SCRIPT_SRC_PATH=$(cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P)
SCRIPT_SRC_PATH=$(realpath -P $(dirname -- "$0"))
VIM_PATH=$SCRIPT_SRC/vim
VIM_COMPLETION_PATH=$VIM_PATH/pack/completion/start
VIM_PLUGINS_PATH=$VIM_PATH/pack/plugins/start

echo -e "\nFetching Vim8 Plugins...\n"

# Without Git, we can't fetch the plugins, so we ask the user to install it and
# then come back and try again.

if [ ! $(command -v git) ];
then
    echo "Git is required to fetch the Vim 8 plugins but needs to be installed."
    exit 1
fi

# We check for the Vim directory separately from the pack and plugins ones
# just for the sake of giving more information if it's also missing.

check_for_dir_and_create $VIM_PATH "Vim Directory"

# Next, we check for the 'completion' and 'plugins' subdirectories, and we create
# them if they're not there yet.

check_for_dir_and_create $VIM_COMPLETION_PATH "Vim Packs Completion"
check_for_dir_and_create $VIM_PLUGINS_PATH "Vim Packs Plugins"

# Now, we get the repos. Let's start with the Completion ones:
#   * Clang Complete

check_for_repo_and_clone $VIM_COMPLETION_PATH \
                         https://github.com/xavierd/clang_complete.git \
                         "Clang Complete"

# Now, we get the Plugins repos:
#   Omnisharp Vim
#   Surround
#   Tagbar
#   Vim Cpp Modern

check_for_repo_and_clone $VIM_PLUGINS_PATH \
                         https://github.com/Omnisharp/omnisharp-vim.git \
                         "Omnisharp Vim"

check_for_repo_and_clone $VIM_PLUGINS_PATH \
                         https://tpope.io/vim/surround.git \
                         "Surround"

check_for_repo_and_clone $VIM_PLUGINS_PATH \
                         https://github.com/preservim/tagbar.git \
                         "Tagbar"

check_for_repo_and_clone $VIM_PLUGINS_PATH \
                         https://github.com/bfrg/vim-cpp-modern.git \
                         "Vim Cpp Modern"

check_for_repo_and_clone $VIM_PLUGINS_PATH \
                         https://tpope.io/vim/fugitive.git \
                         "Fugitive"

