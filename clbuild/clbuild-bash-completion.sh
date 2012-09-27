#!/bin/bash

# Dublicated from the main clbuild script
clbuild_dir=$(pwd)

project_files="projects wnpp-projects my-projects implementations"

project_groups="--all-projects --main-projects --wnpp-projects --installed"

applications="clim-launcher listener gsharp climacs closure beirc climplayer
demodemo clim-alerts eclipse hunchentoot webdav parse-xml validate-xml
valideate-relax-ng html-to-xhtml xhtml-to-html xuriella vecto-demo
adw-charting-demo ltk-demo clpython"

clbuild_commands="help update install uninstall diff slime lisp preloaded check\
                  list recompile record-dependencies run show"

global_options=" --help --implementation --long-help"

cmd=""
function _clbuild_set_cmd {
    # somewhat at-hoc: breaks when strange options are used, but does the job
    # otherwise
    for arg in "$@"; do
        if echo "$clbuild_commands" | grep -- "\b$arg\b" > /dev/null; then
            cmd=$arg
        fi
    done
}

function _clbuildcomp {
    local cur="${COMP_WORDS[COMP_CWORD]}"
    COMPREPLY=( $(compgen -W  "$1" -- ${cur}) )
}

function _clbuild_projects {
    local include_groups="$1"
    local projects=$(cut -d' ' -f1 $(for proj in ${project_files}; do echo ${clbuild_dir}/${proj}; done) | grep -ve '^#' | grep -v '^$')

    # include project groups if the first parameter is not empty
    if [ -n "$include_groups" ]; then
	projects="${project_groups} ${projects}"
    fi
    _clbuildcomp "${projects}"
}

function _clbuild_commands {
    _clbuildcomp "${clbuild_commands}"
}

function _clbuild_implementations {
    local implementations
    implementations=$(cut -d' ' -f1 ${clbuild_dir}/implementations | grep -v '^#' | grep -v '^$')
    _clbuildcomp "${implementations}"
}

function _clbuild_applications {
    _clbuildcomp "${applications}"
}

function _clbuild_completion {
    local cur prev
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    # set the `cmd' variable.
    _clbuild_set_cmd "$@"
    case $prev in
        --implementation)
            _clbuild_implementations
            ;;
        update|install|uninstall)
            _clbuild_projects "+groups"
            ;;
	recompile|show)
	    _clbuild_projects
	    ;;
        run)
            _clbuild_applications
            ;;
        *clbuild)
	    _clbuildcomp "${global_options} ${clbuild_commands}"
            ;;
        *)
            if [ -n $cmd ]; then
                _clbuild_commands
            fi
            ;;
    esac
    return 0
}
        
complete -F _clbuild_completion clbuild
