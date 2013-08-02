#ifndef KCORE_DX_H
#define KCORE_DX_H

#include <d3d9.h>
#include <d3dx9.h>

#ifndef KCORE_SELF_BUILD
#ifdef _DEBUG
#pragma comment(lib, "d3dx9d.lib")
#else
#pragma comment(lib, "d3dx9.lib")
#endif

#pragma comment(lib, "d3d9.lib")
#pragma comment(lib, "dxguid.lib")
#pragma comment(lib, "DxErr.lib")
#endif

enum PCI_Vendors
{
	PCIV_ATI    = 0x1002,
	PCIV_nVidia = 0x10DE,
	PCIV_Intel  = 0x8086
};

typedef HRESULT (*KCORE_CREATE_DEVICE_CALLBACK)(IDirect3DDevice9* pd3dDevice);
typedef void (*KCORE_FRAME_RENDER_CALLBACK)(IDirect3DDevice9* pd3dDevice);
typedef void (*KCORE_DESTROY_DEVICE_CALLBACK)(IDirect3DDevice9* pd3dDevice);

#define KSAFE_RELEASE(p) if (p) { p->Release(); p = NULL; }

void KCDXSetCallbackCreateDevice(KCORE_CREATE_DEVICE_CALLBACK cb);
void KCDXSetCallbackFrameRender(KCORE_FRAME_RENDER_CALLBACK cb);
void KCDXSetCallbackDeviceDestroyed(KCORE_DESTROY_DEVICE_CALLBACK cb);

void KCDXInit();
void KCDXCreateWindow(const char *title, int width, int height);
void KCDXCreateDevice();

void KCDXMainLoop();

IDirect3DDevice9* KCDXGetDevice();

void KCDX_HR_FAILCHECK(DWORD hr, const char *desc);

#endif
