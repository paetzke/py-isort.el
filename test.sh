#! /bin/bash


install_emacs24() {
    sudo add-apt-repository ppa:cassou/emacs -y
    sudo apt-get update -y

    sudo apt-get install emacs24 -y
}


on_error() {
    local msg=$1

    echo $msg
    exit 1
}


test_01() {
    emacs --no-init-file --load py-isort.el -nw ./test_data/test_01/before.py \
          -f py-isort-before-save \
          -f save-buffer \
          -f save-buffers-kill-terminal
    diff ./test_data/test_01/before.py ./test_data/test_01/after.py

    if [ $? != 0 ]; then
        on_error "test_01"
    fi
}


test_02() {
    emacs --no-init-file --load py-isort.el -nw ./test_data/test_02/before.py \
          -f py-isort-before-save \
          -f save-buffer \
          -f save-buffers-kill-terminal
    diff ./test_data/test_02/before.py ./test_data/test_02/after.py

    if [ $? != 0 ]; then
        on_error "test_02"
    fi
}


main() {
    if [ "$TRAVIS" = "true" ]; then
        install_emacs24
    fi

    test_01
    test_02
}


main
