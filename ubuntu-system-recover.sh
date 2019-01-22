#!/bin/bash

sudo apt-get update
sudo apt-get upgrade


sudo apt-get install -y emacs
sudo apt-get install -y vim-gtk3

sudo apt-get install -y build-essential
sudo apt-get install -y cpp-doc gcc-7-locales debian-keyring g++-multilib g++-7-multilib gcc-7-doc
sudo apt-get install -y libstdc++6-7-dbg gcc-multilib autoconf automake libtool flex bison gcc-doc
sudo apt-get install -y gcc-7-multilib libgcc1-dbg libgomp1-dbg libitm1-dbg libatomic1-dbg libasan4-dbg
sudo apt-get install -y liblsan0-dbg libtsan0-dbg libubsan0-dbg libcilkrts5-dbg libmpx2-dbg libquadmath0-dbg glibc-doc bzr libstdc++-7-doc make-do
sudo apt install -y sshfs texlive-full
sudo apt install -y zsh zsh-antigen nasm asl-tools

# clang
sudo apt-get install -y clang-6.0

# ssh server
sudo apt-get install -y openssh-server openssl

# terminator
sudo apt-get install -y terminator

# python
sudo apt-get install -y python2.7 python3 python-pip python3-pip python-virtualenv
sudo pip install virtualenvwrapper
sudo apt-get -y install python-ipython python3-ipython
sudo apt-get -y install ipython ipython3

sudo apt-get install -y apt-transport-https curl gnupg2
sudo apt-get install -y cmake ninja-build

sudo apt-get install -y qemu qemu-kvm qemu-guest-agent
sudo apt-get build-dep -y qemu

sudo apt-get install -y libgtk-3-dev

sudo apt-get install -y libwebkitgtk-3.0-dev
sudo apt-get install -y libwebkitgtk-dev
sudo apt-get install -y libwebkit2gtk-4.0-dev
sudo apt-get install -y libtiff-dev libgif-dev libxpm-dev
sudo apt-get install -y mailutils
sudo apt-get install -y silversearcher-ag


# sogou pinyin
sudo apt-get install fcitx
sudo apt-get install fcitx-config-gtk
sudo apt-get install fcitx-table-all
sudo apt-get install im-switch

# language packages
sudo apt-get install language-pack-ja
sudo apt-get install language-pack-zh
