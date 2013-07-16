// RacorX
//  1. rendering using vertex shader (#define MY_USE_SHADER)
//  2. fix-function rendering without vertex shader

#include <KCore.h>

#define MY_USE_SHADER


#define SCREEN_WIDTH    640
#define SCREEN_HEIGHT   480

struct CUSTOMVERTEX
{
	FLOAT x, y, z;
	DWORD color;
};
#define CUSTOMVERTEX_FVF (D3DFVF_XYZ|D3DFVF_DIFFUSE)

CUSTOMVERTEX Vertices[] =
{
	{ -1.0f, -1.0f, 0.0f, 0xffff0000 },  // 启用 shader 的时候，没有使用这里的 color
	{  1.0f, -1.0f, 0.0f, 0xffff0000 },
	{ -1.0f,  1.0f, 0.0f, 0xffff0000 },
	{  1.0f,  1.0f, 0.0f, 0xffff0000 },
};


WORD Indices[] = {0, 1, 2, 2, 1, 3};

D3DVERTEXELEMENT9 DeclElements[] = {
	{0, 0, D3DDECLTYPE_FLOAT3, 0, D3DDECLUSAGE_POSITION, 0},
	D3DDECL_END(),
};

// reg c4-7 = WorldViewProj matrix
// reg c8   = constant color
// reg v0   = input register (position here)
const char VertexShaderAsm[] =
"vs.1.1                // shader verion 1.1               \n" \
"dcl_position v0                                          \n" \
"dp4 oPos.x, v0, c4    // oPos = localPos * WorldViewProj \n" \
"dp4 oPos.y, v0, c5                                       \n" \
"dp4 oPos.z, v0, c6                                       \n" \
"dp4 oPos.w, v0, c7                                       \n" \
"mov oD0, c8           // material color = c8             \n";

// global vars
#ifdef MY_USE_SHADER
IDirect3DVertexShader9* g_pVS;
IDirect3DVertexDeclaration9* g_pDecl;
#endif
IDirect3DVertexBuffer9* g_pVB;
IDirect3DIndexBuffer9*  g_pIB;

HRESULT OnDeviceCreate(IDirect3DDevice9* pd3dDevice)
{
	pd3dDevice->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
	pd3dDevice->SetRenderState(D3DRS_LIGHTING, FALSE);

#ifdef MY_USE_SHADER
	HRESULT hr;
	LPD3DXBUFFER pVSBuffer, pErrBuffer;
	hr = D3DXAssembleShader(VertexShaderAsm, sizeof(VertexShaderAsm)-1,
				NULL, NULL, 0, &pVSBuffer, &pErrBuffer);
	if (FAILED(hr))
	{
		OutputDebugString("D3DXAssembleShader Err: ");
		OutputDebugString((char*)pErrBuffer->GetBufferPointer());
		OutputDebugString("\n");
		return hr;
	}

	hr = pd3dDevice->CreateVertexDeclaration(DeclElements, &g_pDecl);
	KCDX_HR_FAILCHECK(hr, "pd3dDevice->CreateVertexDeclaration");

	hr = pd3dDevice->CreateVertexShader((DWORD*)pVSBuffer->GetBufferPointer(), &g_pVS);
	KCDX_HR_FAILCHECK(hr, "pd3dDevice->CreateVertexShader");

	KSAFE_RELEASE(pVSBuffer);
#endif

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

#ifdef MY_USE_SHADER
		pd3dDevice->SetVertexShader(g_pVS);
		pd3dDevice->SetVertexDeclaration(g_pDecl);
		
		D3DXMATRIX matWorldViewProj;
		D3DXMatrixTranspose(&matWorldViewProj, &(matWorld * matView * matProj));
		pd3dDevice->SetVertexShaderConstantF(4, matWorldViewProj, 4);

		FLOAT fMaterial[] = {0, 1, 0, 0};
		pd3dDevice->SetVertexShaderConstantF(8, fMaterial, 1);
#else
		pd3dDevice->SetFVF(CUSTOMVERTEX_FVF);		// 启用fix-function的关键

		pd3dDevice->SetTransform(D3DTS_WORLD, &matWorld);
		pd3dDevice->SetTransform(D3DTS_VIEW, &matView);
		pd3dDevice->SetTransform(D3DTS_PROJECTION, &matProj);
#endif

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
#ifdef MY_USE_SHADER
	KSAFE_RELEASE(g_pVS);
	KSAFE_RELEASE(g_pDecl);
#endif
	KSAFE_RELEASE(g_pIB);
	KSAFE_RELEASE(g_pVB);
}

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR lpCmdLine, int nCmdShow)
{
	KCDXSetCallbackCreateDevice( OnDeviceCreate );
	KCDXSetCallbackFrameRender( OnFrameRender );
	KCDXSetCallbackDeviceDestroyed( OnDestroyDevice );

	KCDXInit();
	KCDXCreateWindow("RacorX DX9", SCREEN_WIDTH, SCREEN_HEIGHT);
	KCDXCreateDevice();

	KCDXMainLoop();

	return 0;
}
