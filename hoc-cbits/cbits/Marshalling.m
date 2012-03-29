#include <Foundation/NSString.h>

const char *nsStringToUTF8(NSString *str)
{
    return [str UTF8String];
}

NSString *utf8ToNSString(const char* cstr)
{
    return [[NSString alloc] initWithUTF8String: cstr];
}
