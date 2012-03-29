#ifndef __Statistics_h__
#define __Statistics_h__

enum
{
    kHOCAboutToEnterHaskell = 0,
    kHOCEnteredHaskell,
    kHOCImportedArguments,
    kHOCAboutToExportResult,
    kHOCAboutToLeaveHaskell,
    kHOCLeftHaskell
};
void recordHOCEvent(int what, void ** args);

#endif /* __Statistics_h__ */
