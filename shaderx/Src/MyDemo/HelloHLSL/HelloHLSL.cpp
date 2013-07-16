// HelloHLSL
//  1. HLSL vs/ps, simple usage

#include <KCore.h>

#define SCREEN_WIDTH    640
#define SCREEN_HEIGHT   480

struct CUSTOMVERTEX
{
	FLOAT x, y, z;
	DWORD color;
};

CUSTOMVERTEX Vertices[] =
{
	{ -1.0f, -1.0f, 0.0f, 0xffff0000 },
	{  1.0f, -1.0f, 0.0f, 0xff00ff00 },
	{ -1.0f,  1.0f, 0.0f, 0xff0000ff },
	{  1.0f,  1.0f, 0.0f, 0xffff0000 },
};


WORD Indices[] = {0, 1, 2, 2, 1, 3};

D3DVERTEXELEMENT9 DeclElements[] = {
	{0, 0, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
	{0, 12, D3DDECLTYPE_D3DCOLOR, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_COLOR, 0},
	D3DDECL_END(),
};

const char VSShaderString[] =
"float4x4 WorldViewProj;								\n" \
"struct VS_INPUT										\n" \
"{														\n" \
"	float4 Pos  : POSITION0;							\n" \
"	float4 Diff : COLOR0;								\n"	\
"};														\n" \
"struct VS_OUTPUT										\n" \
"{														\n" \
"	float4 Pos  : POSITION0;							\n" \
"	float4 Diff : COLOR0;								\n"	\
"};														\n" \
"														\n" \
"VS_OUTPUT VSShaderMain(VS_INPUT Input)					\n" \
"{														\n" \
"	VS_OUTPUT Out = (VS_OUTPUT)0;						\n" \
"	Out.Pos  = mul(Input.Pos, WorldViewProj);			\n" \
"	Out.Diff = Input.Diff;								\n" \
"	return Out;											\n" \
"}														\n";

const char PSShaderString[] =
"float4 PSShaderMain(float4 inColor : COLOR0) : COLOR0	\n" \
"{														\n" \
"	inColor.r *= 0.1;									\n" \
"	return inColor;										\n" \
"}														\n";

// global vars
IDirect3DVertexShader9* g_pVS;
ID3DXConstantTable*     g_pVSConstantTable;
IDirect3DPixelShader9*  g_pPS;
IDirect3DVertexDeclaration9* g_pDecl;
IDirect3DVertexBuffer9* g_pVB;
IDirect3DIndexBuffer9*  g_pIB;

HRESULT OnDeviceCreate(IDirect3DDevice9* pd3dDevice)
{
	HRESULT hr;

	pd3dDevice->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
	pd3dDevice->SetRenderState(D3DRS_LIGHTING, FALSE);

	// decl
	hr = pd3dDevice->CreateVertexDeclaration(DeclElements, &g_pDecl);
	KCDX_HR_FAILCHECK(hr, "pd3dDevice->CreateVertexDeclaration");

	// vs
	LPD3DXBUFFER pShaderBuffer, pErrBuffer;
	hr = D3DXCompileShader(VSShaderString, strlen(VSShaderString), NULL, NULL,
		"VSShaderMain", "vs_1_1", D3DXSHADER_DEBUG, &pShaderBuffer, &pErrBuffer, &g_pVSConstantTable);
	if (FAILED(hr))
	{
		OutputDebugString("D3DXCompileShader Err (vs): ");
		OutputDebugString((char*)pErrBuffer->GetBufferPointer());
		OutputDebugString("\n");
		return hr;
	}

	hr = pd3dDevice->CreateVertexShader((DWORD*)pShaderBuffer->GetBufferPointer(), &g_pVS);
	KCDX_HR_FAILCHECK(hr, "pd3dDevice->CreateVertexShader");

	KSAFE_RELEASE(pShaderBuffer);

	// ps
	hr = D3DXCompileShader(PSShaderString, strlen(PSShaderString), NULL, NULL,
		"PSShaderMain", "ps_2_0", D3DXSHADER_DEBUG, &pShaderBuffer, &pErrBuffer, NULL);
	if (FAILED(hr))
	{
		OutputDebugString("D3DXCompileShader Err (ps): ");
		OutputDebugString((char*)pErrBuffer->GetBufferPointer());
		OutputDebugString("\n");
		return hr;
	}

	hr = pd3dDevice->CreatePixelShader((DWORD*)pShaderBuffer->GetBufferPointer(), &g_pPS);
	KCDX_HR_FAILCHECK(hr, "pd3dDevice->CreatePixelShader");

	KSAFE_RELEASE(pShaderBuffer);

	
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
		D3DXMATRIX matWorld, matView, matProj;
		
		D3DXMatrixIdentity(&matWorld);
		
		D3DXVECTOR3 vEyePt( 0.0f, 0.0f, -5.0f );
		D3DXVECTOR3 vLookatPt( 0.0f, 0.0f, 0.0f );
		D3DXVECTOR3 vUpVec( 0.0f, 1.0f, 0.0f );
		D3DXMatrixLookAtLH( &matView, &vEyePt, &vLookatPt, &vUpVec );

		float aspectRatio = ((float)SCREEN_WIDTH) / ((float)SCREEN_HEIGHT);
		D3DXMatrixPerspectiveFovLH( &matProj, D3DX_PI/4, aspectRatio, 1.0f, 100.0f );

		pd3dDevice->SetVertexShader(g_pVS);
		pd3dDevice->SetPixelShader(g_pPS);
		pd3dDevice->SetVertexDeclaration(g_pDecl);
		
		D3DXMATRIX matWorldViewProj;
		D3DXMatrixTranspose(&matWorldViewProj, &(matWorld * matView * matProj));

		D3DXHANDLE hMatrix = g_pVSConstantTable->GetConstantByName(NULL, "WorldViewProj");
		g_pVSConstantTable->SetMatrix(pd3dDevice, hMatrix, &matWorldViewProj);

		FLOAT fMaterial[] = {0, 1, 0, 0};
		pd3dDevice->SetVertexShaderConstantF(8, fMaterial, 1);

		pd3dDevice->SetStreamSource(0, g_pVB, 0, sizeof(CUSTOMVERTEX));
		pd3dDevice->SetIndices(g_pIB);
		pd3dDevice->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, 4, 0, 2);

		pd3dDevice->EndScene();
	}

	pd3dDevice->Present(NULL, NULL, NULL, NULL);
}

void OnDestroyDevice(IDirect3DDevice9* pd3dDevice)
{
	KSAFE_RELEASE(g_pVSConstantTable);
	KSAFE_RELEASE(g_pVS);
	KSAFE_RELEASE(g_pPS);
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
	KCDXCreateWindow("HelloHLSL DX9", SCREEN_WIDTH, SCREEN_HEIGHT);
	KCDXCreateDevice();

	KCDXMainLoop();

	return 0;
}
