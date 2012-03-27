#include "FFICallInterface.h"
#include "Log.h"
#include <stdlib.h>

ffi_cif * allocCif()
{
    return (ffi_cif*) calloc(1,sizeof(ffi_cif));
}

ffi_abi defaultABI()
{
    return FFI_DEFAULT_ABI;
}

ffi_type * allocStructType(ffi_type **elements)
{
    ffi_type *theStruct = (ffi_type *) malloc(sizeof(ffi_type));
    theStruct->type = FFI_TYPE_STRUCT;
    theStruct->size = theStruct->alignment = 0;
    theStruct->elements = elements;
    
    return theStruct;
}

int cifIsStret(ffi_cif *cif)
{
    int ret;
    
    if(cif->rtype->type == FFI_TYPE_STRUCT)
    {
#ifdef __i386__
            // on Darwin/x86, structs 8 bytes and smaller are returned
            // in registers, and we have to use objc_msgSend and not
            // objc_msgSend_stret.
        ret = cif->rtype->size > 8;
#elif __x86_64__
            // on x86_64, the rules are more complicated.  If I'm reading the
            // ABI spec[1] properly, a structure is passed in memory if:
            //
            //      It is larger than 32 bytes
            // OR   It contains unaligned members (not enough info here to know!
            //          in fact, it's not really even clear whether the spec means
            //          aligned for the type of the member or "eightbyte"-aligned)
            // OR   It is a C++ object with a "non-trivial" copy constructor or
            //          deconstructor.
            // OR   It satisfies some utterly incomprehensible criteria relating
            //          to C++ objects (can someone please decipher section 4
            //          of the complex argument classification section of the 
            //          ABI spec?  In particular, "Each field of an object is 
            //          classified recursively so that always two fields are 
            //          considered" makes no sense to me at all.)
            // OR (     It is larger than 8 bytes
            //      AND any one of the 8 byte slices would be passed in memory
            //      AND the structure is not _m256 or long double
            //    )
            // OR   It is variable-sized[2]
            // OR   It is a complex tetra-integer or tetra-float or any vector
            //          type taking > 16 bytes[2].  I don't see from the spec why
            //          these should be the case though...  Clause 5(c) maybe?
            //
            // All that goes out the window if you're on a Mac though - their
            // GCC passes structs over 16 bytes in memory[3].
            //
            //  [1] http://www.x86-64.org/documentation/abi-0.99.pdf
            //  [2] classify_argument in gcc/config/i386/i386.c in GCC sources
            //  [3] same source in llvm-gcc 4.2 sources as of XCode 4.3
        
        // TODO: this logic is not complete, and it will probably not be easy to complete it.
        ret = cif->rtype->size > 16;
#else
        ret = 1;
#endif
    }
    else
        ret = 0;
    
    #if DO_LOG
    printf("cifIsStret(%p) -> %d (cif->rtype->size = %d)\n", cif, ret, (int) cif->rtype->size);
    #endif
    
    return ret;
}
