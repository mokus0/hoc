function build()
{
    ( cd $1 && cabal install $ARGUMENTS; )
}

ARGUMENTS="$@"

set -e
mkdir -p Generated
cd Generated

export GNUSTEP_SYSTEM_HEADERS="$(gnustep-config --variable=GNUSTEP_SYSTEM_HEADERS)"

hoc-ifgen Foundation -f -b ../binding-script.txt -a ../AdditionalCode/ --headers=$GNUSTEP_SYSTEM_HEADERS/GNUstepBase/
build HOC-Foundation
hoc-ifgen AppKit -f -b ../binding-script.txt -a ../AdditionalCode/ -d Foundation -d GNUstepBase --headers=$GNUSTEP_SYSTEM_HEADERS/GNUstepGUI/
build HOC-AppKit
hoc-ifgen Cocoa -u -d Foundation -d AppKit
build HOC-Cocoa

