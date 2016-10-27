#!/bin/bash

set -xe

top=`pwd`

cd PackedTreesFormal
stack test
