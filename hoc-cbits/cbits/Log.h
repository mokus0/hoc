#ifndef __Log_h__
#define __Log_h__

#ifndef DO_LOG
#define DO_LOG 0
#endif

#if DO_LOG
#include <stdio.h> // for printf
#include <Foundation/Foundation.h> // for NSLog
#endif

#endif /* __Log_h__ */
