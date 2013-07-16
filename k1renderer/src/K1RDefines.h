#ifndef K1R_DEFINES_H
#define K1R_DEFINES_H


// platform
#if defined(_MSC_VER)
    #define K1R_WIN32      1
    #define K1R_LINUX      0
#elif defined(__GNUC__)
	#define K1R_WIN32      0
	#define K1R_LINUX      1
#else
    #error	platform symbol must be defined.
#endif


// compiler
#if defined(_MSC_VER)
	#define K1R_MSVC       1
	#define K1R_GCC        0
#elif defined(__GNUC__)
	#define K1R_MSVC       0
	#define K1R_GCC        1
#else
    #error  MS Visual C++ or GCC needed
#endif


#endif