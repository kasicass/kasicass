#ifndef K1R_RENDERER_DX9DEFINES_H
#define K1R_RENDERER_DX9DEFINES_H

#include <d3d9.h>
#include <d3dx9.h>

namespace K1R { namespace DX9 {

typedef LPDIRECT3D9				DeviceMaker;
typedef LPDIRECT3DDEVICE9		Device;
typedef LPDIRECT3DVERTEXBUFFER9	VertexBuffer;
typedef LPDIRECT3DINDEXBUFFER9  IndexBuffer;
typedef LPDIRECT3DVERTEXSHADER9 VertexShader;
typedef LPDIRECT3DPIXELSHADER9  PixelShader;
typedef LPD3DXCONSTANTTABLE     ShaderConstantTable;

template <class T>
inline void SafeRelease(T*& p)
{
	if (p) p->Release();
	p = NULL;
}

Device GetDevice();
void SetDevice(Device dev);

} }

#endif