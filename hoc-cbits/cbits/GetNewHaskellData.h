#ifndef __GetNewHaskellData_h__
#define __GetNewHaskellData_h__

#include "Common.h"
#include <HsFFI.h>

HsStablePtr getNewHaskellDataForClass(id obj, Class isa);
HsStablePtr getNewHaskellData(id obj);

#endif /* __GetNewHaskellData_h__ */
