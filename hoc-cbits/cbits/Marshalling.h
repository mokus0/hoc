#ifndef __Marshalling_h__
#define __Marshalling_h__

@class NSString;

const char *nsStringToUTF8(NSString *);
NSString *utf8ToNSString(const char*);

#endif /* __Marshalling_h__ */
