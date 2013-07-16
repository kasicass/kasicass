// RacorX3
//  1. per-vertex lighting, color = N * L
//  2. light position (0.0, 0.0, 1.0)

#define KCORE_USE_DX9
#include <KCore.h>

#define SCREEN_WIDTH    640
#define SCREEN_HEIGHT   480

#define CLIP_MATRIX            0
#define CLIP_MATRIX_1          1
#define CLIP_MATRIX_2          2
#define CLIP_MATRIX_3          3
#define INVERSE_WORLD_MATRIX   4
#define INVERSE_WORLD_MATRIX_1 5
#define INVERSE_WORLD_MATRIX_2 6
#define LIGHT_POSITION         7
#define DIFFUSE_COLOR          14
#define LIGHT_COLOR            15

struct CUSTOMVERTEX
{
	FLOAT x, y, z;
	FLOAT nx, ny, nz;
};
#define CUSTOMVERTEX_FVF (D3DFVF_XYZ|D3DFVF_NORMAL)

CUSTOMVERTEX Vertices[] =
{
	{ -1.0f, -1.0f, 0.0f, 0.0f, 0.0f, -1.0f, },
	{  1.0f, -1.0f, 0.0f, 0.0f, 0.0f, -1.0f, },
	{ -1.0f,  1.0f, 0.0f, 0.0f, 0.0f, -1.0f, },
	{  1.0f,  1.0f, 0.0f, 0.0f, 0.0f, -1.0f, },
};


WORD Indices[] = {0, 1, 2, 2, 1, 3};

D3DVERTEXELEMENT9 DeclElements[] = {
	{0, 0, D3DDECLTYPE_FLOAT3, 0, D3DDECLUSAGE_POSITION, 0},
	{0, 12, D3DDECLTYPE_FLOAT3, 0, D3DDECLUSAGE_NORMAL, 0},
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
	KCReadFileContent(KResMgr::GetResPath("ShaderX/RacorX3/diffuse_dx9.vso"),
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
		
		FLOAT fRadian = (GetTickCount() / 100.0f) * (3.14159f/360*5);
		D3DXMatrixRotationY(&matWorld, fRadian);
		
		D3DXVECTOR3 vEyePt( 0.0f, 0.0f, -5.0f );
		D3DXVECTOR3 vLookatPt( 0.0f, 0.0f, 0.0f );
		D3DXVECTOR3 vUpVec( 0.0f, 1.0f, 0.0f );
		D3DXMatrixLookAtLH( &matView, &vEyePt, &vLookatPt, &vUpVec );

		float aspectRatio = ((float)SCREEN_WIDTH) / ((float)SCREEN_HEIGHT);
		D3DXMatrixPerspectiveFovLH( &matProj, D3DX_PI/4, aspectRatio, 1.0f, 100.0f );

		pd3dDevice->SetVertexShader(g_pVS);
		pd3dDevice->SetVertexDeclaration(g_pDecl);
		
		//D3DXMATRIX matWorldViewProj;
		//D3DXMatrixTranspose(&matWorldViewProj, &(matWorld * matView * matProj));
		pd3dDevice->SetVertexShaderConstantF(CLIP_MATRIX, matWorld * matView * matProj, 4);

		D3DXMATRIX matWorldInverse;
		D3DXMatrixInverse(&matWorldInverse, NULL, &matWorld);
		pd3dDevice->SetVertexShaderConstantF(INVERSE_WORLD_MATRIX, matWorldInverse, 3);

		FLOAT fLightPos[] = {0, 0, 1, 0};
		pd3dDevice->SetVertexShaderConstantF(LIGHT_POSITION, fLightPos, 1);

		FLOAT fDiffuse[] = {0, 1, 0, 0};
		pd3dDevice->SetVertexShaderConstantF(DIFFUSE_COLOR, fDiffuse, 1);

		FLOAT fLight[] = {1, 1, 1, 0};
		pd3dDevice->SetVertexShaderConstantF(LIGHT_COLOR, fLight, 1);

		pd3dDevice->SetStreamSource(0, g_pVB, 0, sizeof(CUSTOMVERTEX));
		pd3dDevice->SetIndices(g_pIB);
		pd3dDevice->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, 4, 0, 2);

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
