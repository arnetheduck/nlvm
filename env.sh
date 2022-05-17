#!/bin/env bash

REL_PATH="$(dirname ${BASH_SOURCE[0]:-${(%):-%x}})"
ABS_PATH="$(cd ${REL_PATH}; pwd)"

export PATH=$ABS_PATH/Nim/bin:$PATH

if [[ $# == 1 && $1 == "bash" ]]; then
        # the only way to change PS1 in a child shell, apparently
# (we're not getting the original PS1 value in here, so set a complete and nice prompt)
        export PS1="[Nimbus env] \[\033[0;31m\]\l \[\033[1;33m\]\d \[\033[1;36m\]\t \[\033[0;32m\]|\w|\[\033[0m\]\n\u\$ "
        exec "$1" --login --noprofile
else
        # can't use "exec" here if we're getting function names as params
        "$@"
fi

