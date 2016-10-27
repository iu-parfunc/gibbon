#!/bin/bash

set -xe

top=`pwd`

hostname
uname -a
which -a stack
stack --version

cd PackedTreesFormal
stack test
