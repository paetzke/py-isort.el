#! /bin/bash

function install_emacs24 {
    sudo add-apt-repository ppa:cassou/emacs -y
    sudo apt-get update -y

    sudo apt-get install emacs24 -y
}


function install_package {
    emacs -nw py-isort.el -f package-install-from-buffer -f kill-emacs
}


function test_01 {
    emacs -nw ./test_data/test_01_before.py -f py-isort-before-save -f save-buffer -f save-buffers-kill-terminal
    diff ./test_data/test_01_before.py ./test_data/test_01_after.py
    if [ $? != 0 ]; then
        exit 1
    fi
}


function test_02 {
    emacs -nw ./test_data/test_02/test_02_before.py -f py-isort-before-save -f save-buffer -f save-buffers-kill-terminal
    diff ./test_data/test_02/test_02_before.py ./test_data/test_02/test_02_after.py
    if [ $? != 0 ]; then
        exit 1
    fi
}


function main {
    install_emacs24
    install_package

    test_01
    test_02
}


main
