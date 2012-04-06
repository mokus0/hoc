#ifndef __ObjectMap_h__
#define __ObjectMap_h__

#include <HsFFI.h>

HsStablePtr getHaskellPart(id objcPart);
void setHaskellPart(id objcPart, HsStablePtr haskellPart, HsBool immortal);
void removeHaskellPart(id objcPart, HsStablePtr haskellPart);

void objectMapStatistics(unsigned *allocated, unsigned *immortal);

#endif /* __ObjectMap_h__ */
