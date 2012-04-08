#ifndef __RetainedHaskellPart_h__
#define __RetainedHaskellPart_h__

#include "Common.h"
#include <HsFFI.h>

HsStablePtr getRetainedHaskellPart(id obj);
void setRetainedHaskellPart(id obj, HsStablePtr haskellPart);

#endif /* __RetainedHaskellPart_h__ */
