#ifndef __RetainedHaskellPart_h__
#define __RetainedHaskellPart_h__

#include "Common.h"

void* getRetainedHaskellPart(id obj);
void setRetainedHaskellPart(id obj, void* haskellPart);

#endif /* __RetainedHaskellPart_h__ */
