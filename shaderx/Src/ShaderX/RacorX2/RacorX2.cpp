// RacorX2
//  1. 从编译的.vso文件加载shader,  (nvasm.exe basic.vsh => basic.vso)
//  2. DX9 用 Utilities\Bin\x86\vsa.exe & psa.exe 来编译

#define KCORE_USE_DX9
#include <KCore.h>

#define SCREEN_WIDTH    640
#define SCREEN_HEIGHT   480

struct CUSTOMVERTEX
{
	FLOAT x, y, z;
};
#define CUSTOMVERTEX_FVF (D3DFVF_XYZ)

CUSTOMVERTEX Vertices[] =
{
	{ -1.0f, -1.0f, 0.0f },
	{  1.0f, -1.0f, 0.0f },
	{ -1.0f,  1.0f, 0.0f },
	{  1.0f,  1.0f, 0.0f },
};


WORD Indices[] = {0, 1, 2, 2, 1, 3};

D3DVERTEXELEMENT9 DeclElements[] = {
	{0, 0, D3DDECLTYPE_FLOAT3, 0, D3DDECLUSAGE_POSITION, 0},
	D3DDECL_END(),
};

// global vars
IDirect3DVertexShader9* g_pVS;
IDirect3DVertexDeclaration9* g_pDecl;
IDirect3DVertexBuffer9* g_pVB;
IDirect3DIndexBuffer9*  g_pIB;

HRESULT OnDeviceCreate(IDirect3DDevice9* pd3dDevice)
{
	pd3dDevice->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
	pd3dDevice->SetRenderState(D3DRS_LIGHTING, FALSE);

	HRESULT hr;
	char compiledShaderBuf[1024];
	KCReadFileContent(KResMgr::GetResPath("ShaderX/RacorX2/basic_dx9.vso"),
		compiledShaderBuf, sizeof(compiledShaderBuf));

	hr = pd3dDevice->CreateVertexDeclaration(DeclElements, &g_pDecl);
	KCDX_HR_FAILCHECK(hr, "pd3dDevice->CreateVertexDeclaration");

	hr = pd3dDevice->CreateVertexShader((DWORD*)compiledShaderBuf, &g_pVS);
	KCDX_HR_FAILCHECK(hr, "pd3dDevice->CreateVertexShader");

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
		D3DXMATRIX matWorld, matView, matProj;
		
		D3DXMatrixIdentity(&matWorld);
		
		D3DXVECTOR3 vEyePt( 0.0f, 0.0f, -5.0f );
		D3DXVECTOR3 vLookatPt( 0.0f, 0.0f, 0.0f );
		D3DXVECTOR3 vUpVec( 0.0f, 1.0f, 0.0f );
		D3DXMatrixLookAtLH( &matView, &vEyePt, &vLookatPt, &vUpVec );

		float aspectRatio = ((float)SCREEN_WIDTH) / ((float)SCREEN_HEIGHT);
		D3DXMatrixPerspectiveFovLH( &matProj, D3DX_PI/4, aspectRatio, 1.0f, 100.0f );

		pd3dDevice->SetVertexShader(g_pVS);
		pd3dDevice->SetVertexDeclaration(g_pDecl);
		
		D3DXMATRIX matWorldViewProj;
		D3DXMatrixTranspose(&matWorldViewProj, &(matWorld * matView * matProj));
		pd3dDevice->SetVertexShaderConstantF(4, matWorldViewProj, 4);

		FLOAT fMaterial[] = {0, 1, 0, 0};
		pd3dDevice->SetVertexShaderConstantF(8, fMaterial, 1);

		pd3dDevice->SetStreamSource(0, g_pVB, 0, sizeof(CUSTOMVERTEX));
		pd3dDevice->SetIndices(g_pIB);
		pd3dDevice->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, 4, 0, 2);
		//pd3dDevice->DrawPrimitive(D3DPT_TRIANGLESTRIP, 0, 2);

		pd3dDevice->EndScene();
	}

	pd3dDevice->Present(NULL, NULL, NULL, NULL);
}

void OnDestroyDevice(IDirect3DDevice9* pd3dDevice)
{
	KSAFE_RELEASE(g_pVS);
	KSAFE_RELEASE(g_pDecl);
	KSAFE_RELEASE(g_pIB);
	KSAFE_RELEASE(g_pVB);
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
