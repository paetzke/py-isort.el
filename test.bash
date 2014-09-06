#! /bin/bash

function install_emacs24 {
    sudo add-apt-repository ppa:cassou/emacs -y
    sudo apt-get update -y

    sudo apt-get install emacs24 -y
}


function install_package {
    emacs -nw py-isort.el -f package-install-from-buffer -f kill-emacs
}


function main {
    install_emacs24
    install_package
}


main
