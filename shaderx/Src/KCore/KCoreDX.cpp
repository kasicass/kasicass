#include "KCoreDX.h"
#include "WinUtil.h"
#include <stdio.h>
#include <dxerr.h>

struct DXState
{
	// win stuff
	HWND hWnd;

	// dx9 stuff
	IDirect3DDevice9* pd3dDevice;

	// callback
	KCORE_CREATE_DEVICE_CALLBACK  onCreateDevice;
	KCORE_FRAME_RENDER_CALLBACK   onFrameRender;
	KCORE_DESTROY_DEVICE_CALLBACK onDestroyDevice;
};

static struct DXState g_DXState;


void KCDXSetCallbackCreateDevice(KCORE_CREATE_DEVICE_CALLBACK cb)
{
	g_DXState.onCreateDevice = cb;
}

void KCDXSetCallbackFrameRender(KCORE_FRAME_RENDER_CALLBACK cb)
{
	g_DXState.onFrameRender = cb;
}

void KCDXSetCallbackDeviceDestroyed(KCORE_DESTROY_DEVICE_CALLBACK cb)
{
	g_DXState.onDestroyDevice = cb;
}



//
// Window Message Handler
//

static LRESULT CALLBACK KCDXWndProc (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	switch (message)
	{
	case WM_DESTROY:
		::PostQuitMessage(0);
		return 0;

	case WM_KEYDOWN:
		return 0;

	case WM_LBUTTONDOWN:
		return 0;

	case WM_MOUSEMOVE:
		return 0;
	}

     return DefWindowProc (hWnd, message, wParam, lParam) ;
}

void KCDXInit()
{

}

void KCDXCreateWindow(const char *title, int width, int height)
{
	HINSTANCE hInst = ::GetModuleHandle(NULL);
	
	WinClassMaker clsMaker(hInst, "KCore DX Demo", KCDXWndProc);
	clsMaker.Register();

	WinMaker winMaker(hInst, "KCore DX Demo");
	winMaker.SetSize(width, height);
	g_DXState.hWnd = winMaker.Create(title);
}

void KCDXCreateDevice()
{
	IDirect3D9* pd3d;
	IDirect3DDevice9* pd3dDevice;

	pd3d = Direct3DCreate9(D3D_SDK_VERSION);

    // Create the D3DDevice
	HRESULT hr;
	D3DDISPLAYMODE dm;
	hr = pd3d->GetAdapterDisplayMode(D3DADAPTER_DEFAULT, &dm);
	KCDX_HR_FAILCHECK(hr, "pd3d->EnumAdapterModes");

	// Set up the structure used to create the D3DDevice
    D3DPRESENT_PARAMETERS d3dpp;
    ZeroMemory( &d3dpp, sizeof(d3dpp) );
    d3dpp.Windowed = TRUE;
    d3dpp.SwapEffect = D3DSWAPEFFECT_DISCARD;
    d3dpp.BackBufferFormat = dm.Format;

    hr = pd3d->CreateDevice( D3DADAPTER_DEFAULT, D3DDEVTYPE_REF, g_DXState.hWnd,
                                      D3DCREATE_SOFTWARE_VERTEXPROCESSING,
                                      &d3dpp, &pd3dDevice );
	KCDX_HR_FAILCHECK(hr, "pd3d->CreateDevice");

	KSAFE_RELEASE(pd3d);

	g_DXState.pd3dDevice = pd3dDevice;
	g_DXState.onCreateDevice(pd3dDevice);
}

void KCDXMainLoop()
{
	HWND hWnd = g_DXState.hWnd;

	::ShowWindow(hWnd, SW_SHOWNORMAL);
	::UpdateWindow(hWnd);

	MSG msg;
	ZeroMemory(&msg, sizeof(msg));
	while (msg.message != WM_QUIT)
	{
		if (::PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
		{
			::DispatchMessage(&msg);
		}
		else
		{
			g_DXState.onFrameRender(g_DXState.pd3dDevice);
		}
	}

	g_DXState.onDestroyDevice(g_DXState.pd3dDevice);
	KSAFE_RELEASE(g_DXState.pd3dDevice);
}

IDirect3DDevice9* KCDXGetDevice()
{
	return g_DXState.pd3dDevice;
}

void KCDX_HR_FAILCHECK(DWORD hr, const char *desc)
{
	if (FAILED(hr))
	{
		char finalMsg[512];
		sprintf_s(finalMsg, sizeof(finalMsg), "%s: %s", desc, DXGetErrorString(hr));
		MessageBox(NULL, finalMsg, "Error", MB_OK);
		exit(1);
	}
}
