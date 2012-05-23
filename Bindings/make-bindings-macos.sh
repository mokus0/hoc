#!/bin/bash

ARGS=( "$@" )
OPTS=( )

IFGEN=hoc-ifgen

function generate()
{
    "$IFGEN" "$@" "${OPTS[@]}"
}

function build()
{ (
    set -e
    cd HOC-$1
    cabal configure "${ARGS[@]}"
    cabal build
    cabal install
  )
}

set -e
mkdir -p Generated
cd Generated

generate Foundation -f -b ../binding-script.txt -a ../AdditionalCode/
build Foundation

generate QuartzCore -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation
build QuartzCore

generate AppKit -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation -d QuartzCore
build AppKit

generate CoreData -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation -d AppKit -d QuartzCore
build CoreData

generate Cocoa -u -d Foundation -d QuartzCore -d AppKit -d CoreData
build Cocoa

echo Wahoo.