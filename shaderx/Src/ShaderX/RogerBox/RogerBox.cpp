// RogerBox
//  1. use KRoger framework to draw a box

#define KCORE_USE_DX9
#include <KCore.h>

#define SCREEN_WIDTH    640
#define SCREEN_HEIGHT   480

// global vars
KBox *g_box = NULL;

HRESULT OnDeviceCreate(IDirect3DDevice9* pd3dDevice)
{
	KRoger::init(pd3dDevice, SCREEN_WIDTH, SCREEN_HEIGHT);
	g_box = KRoger::loadObj("RogerBox/box.obj");
	return S_OK;
}

void OnFrameRender(IDirect3DDevice9* pd3dDevice)
{
	KRoger::beginScene();
	g_box->draw();
	KRoger::endScene();
}

void OnDestroyDevice(IDirect3DDevice9* pd3dDevice)
{
	delete g_box;
	KRoger::fini(pd3dDevice);
}

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR lpCmdLine, int nCmdShow)
{
	KCDXSetCallbackCreateDevice( OnDeviceCreate );
	KCDXSetCallbackFrameRender( OnFrameRender );
	KCDXSetCallbackDeviceDestroyed( OnDestroyDevice );

	KCDXInit();
	KCDXCreateWindow("RacorX2 DX9", SCREEN_WIDTH, SCREEN_HEIGHT);
	KCDXCreateDevice();

	KCDXMainLoop();

	return 0;
}
