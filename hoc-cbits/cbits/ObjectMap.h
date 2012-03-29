#ifndef __ObjectMap_h__
#define __ObjectMap_h__

void *getHaskellPart(void* objcPart);
void setHaskellPart(void* objcPart, void* haskellPart, int immortal);
void removeHaskellPart(void* objcPart, void* haskellPart);

void objectMapStatistics(unsigned *allocated, unsigned *immortal);

#endif /* __ObjectMap_h__ */
