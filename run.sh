#!/bin/bash

make

insn=$1
arch=${2:-x86}

echo 'bap-mc:'
echo $insn | bap-mc --arch=$arch --show-bil

echo 'my-model:'
echo $insn | ./run_x86.native --arch=$arch
