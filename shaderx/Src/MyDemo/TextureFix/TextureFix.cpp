// TextureFix - simple texture in fix-function pipeline

#include <KCore.h>

#define SCREEN_WIDTH    640
#define SCREEN_HEIGHT   480

// use D3DFVF_XYZRHW or not
#define USE_RHW
#define USE_RHW_FIXUP

#if defined(USE_RHW)

struct CUSTOMVERTEX
{
	FLOAT x, y, z, w;
	FLOAT u, v;
};
#define FVF_CUSTOMVERTEX  (D3DFVF_XYZRHW | D3DFVF_TEX1)
CUSTOMVERTEX Vertices[] =
{   //   X      Y      Z     W      
#if defined(USE_RHW_FIXUP)
	{   0.0f-0.5f, 480.0f-0.5f, 1.0f, 1.0f, 0.0f, 1.0f },
	{ 640.0f-0.5f, 480.0f-0.5f, 1.0f, 1.0f, 1.0f, 1.0f },
	{   0.0f-0.5f,   0.0f-0.5f, 1.0f, 1.0f, 0.0f, 0.0f },
	{ 640.0f-0.5f,   0.0f-0.5f, 1.0f, 1.0f, 1.0f, 0.0f },
#else
	{   0.0f, 480.0f, 1.0f, 1.0f, 0.0f, 1.0f },
	{ 640.0f, 480.0f, 1.0f, 1.0f, 1.0f, 1.0f },
	{   0.0f,   0.0f, 1.0f, 1.0f, 0.0f, 0.0f },
	{ 640.0f,   0.0f, 1.0f, 1.0f, 1.0f, 0.0f },
#endif
};

#else

struct CUSTOMVERTEX
{
	FLOAT x, y, z;
	FLOAT u, v;
};
#define FVF_CUSTOMVERTEX  (D3DFVF_XYZ | D3DFVF_TEX1)
CUSTOMVERTEX Vertices[] =
{
	{ -1.0f, -1.0f, 0.0f, 0.0f, 1.0f },
	{  1.0f, -1.0f, 0.0f, 1.0f, 1.0f },
	{ -1.0f,  1.0f, 0.0f, 0.0f, 0.0f },
	{  1.0f,  1.0f, 0.0f, 1.0f, 0.0f },
};

#endif

WORD Indices[] = {0, 1, 2, 2, 1, 3};

// global vars
IDirect3DVertexBuffer9* g_pVB;
IDirect3DIndexBuffer9*  g_pIB;
IDirect3DTexture9*      g_pTexture;

HRESULT OnDeviceCreate(IDirect3DDevice9* pd3dDevice)
{
	HRESULT hr;

	pd3dDevice->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
	pd3dDevice->SetRenderState(D3DRS_LIGHTING, FALSE);

	pd3dDevice->SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_NONE);
	pd3dDevice->SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_POINT);  // D3DTEXF_LINEAR
	pd3dDevice->SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_POINT);  // D3DTEXF_LINEAR

	pd3dDevice->SetTextureStageState( 0, D3DTSS_COLOROP,   D3DTOP_SELECTARG1 );
	pd3dDevice->SetTextureStageState( 0, D3DTSS_COLORARG1, D3DTA_TEXTURE );
	
	// texture
	// hr = D3DXCreateTextureFromFile(pd3dDevice, "..\\..\\Media\\MyDemo\\TextureFix\\test.tga", &g_pTexture);
	hr = D3DXCreateTextureFromFileEx(pd3dDevice,
		"..\\..\\Media\\MyDemo\\TextureFix\\test.tga",
		0, // default
		0, // default
		D3DX_DEFAULT_NONPOW2, 
		0, // usage
		D3DFMT_A8R8G8B8, // format
		D3DPOOL_MANAGED, // pool
		D3DX_FILTER_NONE,
		D3DX_FILTER_NONE,
		0,
		NULL,
		NULL,
		&g_pTexture);

	KCDX_HR_FAILCHECK(hr, "D3DXCreateTextureFromFile");

	/*
	D3DSURFACE_DESC desc;
	g_pTexture->GetLevelDesc(0, &desc);
	char buf[80];
	sprintf(buf, "%d\n", desc.Format);
	::MessageBoxA(NULL, buf, "Test", MB_OK);
	*/
	
	// vertex & indices
	pd3dDevice->CreateVertexBuffer(4*sizeof(CUSTOMVERTEX), D3DUSAGE_WRITEONLY, 0, D3DPOOL_MANAGED, &g_pVB, NULL);
	VOID* pVertices;
	g_pVB->Lock(0, sizeof(Vertices), &pVertices, 0);
	memcpy(pVertices, Vertices, sizeof(Vertices));
	g_pVB->Unlock();

	pd3dDevice->CreateIndexBuffer(sizeof(Indices), 0, D3DFMT_INDEX16, D3DPOOL_MANAGED, &g_pIB, NULL);
	VOID *pIndices;
	g_pIB->Lock(0, sizeof(Indices), &pIndices, 0);
	memcpy(pIndices, Indices, sizeof(Indices));
	g_pIB->Unlock();

	return S_OK;
}

void OnFrameRender(IDirect3DDevice9* pd3dDevice)
{
	pd3dDevice->Clear(0, NULL, D3DCLEAR_TARGET, D3DCOLOR_XRGB(0,0,0), 1.0f, 0);

	if (SUCCEEDED(pd3dDevice->BeginScene()))
	{
#if !defined(USE_RHW)
		D3DXMATRIX matWorld, matView, matProj;
		
		D3DXMatrixIdentity(&matWorld);
		pd3dDevice->SetTransform(D3DTS_WORLD, &matWorld);
		
		D3DXVECTOR3 vEyePt( 0.0f, 0.0f, -5.0f );
		D3DXVECTOR3 vLookatPt( 0.0f, 0.0f, 0.0f );
		D3DXVECTOR3 vUpVec( 0.0f, 1.0f, 0.0f );
		D3DXMatrixLookAtLH( &matView, &vEyePt, &vLookatPt, &vUpVec );
		pd3dDevice->SetTransform(D3DTS_VIEW, &matView);

		float aspectRatio = ((float)SCREEN_WIDTH) / ((float)SCREEN_HEIGHT);
		D3DXMatrixPerspectiveFovLH( &matProj, D3DX_PI/4, aspectRatio, 1.0f, 100.0f );
		pd3dDevice->SetTransform(D3DTS_PROJECTION, &matProj);
#endif
		pd3dDevice->SetVertexShader(NULL);
		pd3dDevice->SetPixelShader(NULL);
		pd3dDevice->SetFVF(FVF_CUSTOMVERTEX);
		
		//D3DXMATRIX matWorldViewProj;
		//D3DXMatrixTranspose(&matWorldViewProj, &(matWorld * matView * matProj));
		//pd3dDevice->SetTransform(D3DTS_WORLD, &matWorldViewProj);

		pd3dDevice->SetTexture(0, g_pTexture);

		pd3dDevice->SetStreamSource(0, g_pVB, 0, sizeof(CUSTOMVERTEX));
		pd3dDevice->SetIndices(g_pIB);
		pd3dDevice->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, 4, 0, 2);

		pd3dDevice->EndScene();
	}

	pd3dDevice->Present(NULL, NULL, NULL, NULL);
}

void OnDestroyDevice(IDirect3DDevice9* pd3dDevice)
{
	KSAFE_RELEASE(g_pIB);
	KSAFE_RELEASE(g_pVB);
}

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR lpCmdLine, int nCmdShow)
{
	KCDXSetCallbackCreateDevice( OnDeviceCreate );
	KCDXSetCallbackFrameRender( OnFrameRender );
	KCDXSetCallbackDeviceDestroyed( OnDestroyDevice );

	KCDXInit();
	KCDXCreateWindow("TextureFix DX9", SCREEN_WIDTH, SCREEN_HEIGHT);
	KCDXCreateDevice();

	KCDXMainLoop();

	return 0;
}
