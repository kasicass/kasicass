#ifndef K1R_MATH_MATRIX_H
#define K1R_MATH_MATRIX_H

#include "K1RBase.h"

#if K1R_WIN32
#include <d3dx9.h>
#endif

namespace K1R {

#if K1R_WIN32
	typedef D3DXMATRIX Matrix;
#endif

}

#endif