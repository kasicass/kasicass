// HelloHLSL
//  1. HLSL vs/ps, simple usage

#include <KCore.h>

#define SCREEN_WIDTH    640
#define SCREEN_HEIGHT   480

// global vars
IDirect3DVertexShader9* g_pVS;
ID3DXConstantTable*     g_pVSConstantTable;
IDirect3DPixelShader9*  g_pPS;
KModel*                 g_pModel;

HRESULT OnDeviceCreate(IDirect3DDevice9* pd3dDevice)
{
	HRESULT hr;

	pd3dDevice->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
	pd3dDevice->SetRenderState(D3DRS_LIGHTING, FALSE);

	// vs
	char buf[2048];
	KCReadFileContent(KResMgr::GetResPath("MyDemo/ObjViewer/color_vs.fx"), buf, sizeof(buf));

	LPD3DXBUFFER pShaderBuffer, pErrBuffer;
	hr = D3DXCompileShader(buf, strlen(buf), NULL, NULL,
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
	KCReadFileContent(KResMgr::GetResPath("MyDemo/ObjViewer/color_ps.fx"), buf, sizeof(buf));

	hr = D3DXCompileShader(buf, strlen(buf), NULL, NULL,
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

	// model
	g_pModel = KLoadModel(KResMgr::GetResPath("MyDemo/ObjViewer/tri.obj"));

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
		
		D3DXMATRIX matWorldViewProj;
		D3DXMatrixTranspose(&matWorldViewProj, &(matWorld * matView * matProj));

		D3DXHANDLE hMatrix = g_pVSConstantTable->GetConstantByName(NULL, "WorldViewProj");
		g_pVSConstantTable->SetMatrix(pd3dDevice, hMatrix, &matWorldViewProj);

		KDrawModel(g_pModel);

		pd3dDevice->EndScene();
	}

	pd3dDevice->Present(NULL, NULL, NULL, NULL);
}

void OnDestroyDevice(IDirect3DDevice9* pd3dDevice)
{
	KSAFE_RELEASE(g_pVSConstantTable);
	KSAFE_RELEASE(g_pVS);
	KSAFE_RELEASE(g_pPS);
	KDestroyModel(g_pModel);
}

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR lpCmdLine, int nCmdShow)
{
	KCDXSetCallbackCreateDevice( OnDeviceCreate );
	KCDXSetCallbackFrameRender( OnFrameRender );
	KCDXSetCallbackDeviceDestroyed( OnDestroyDevice );

	KCDXInit();
	KCDXCreateWindow("ObjViewer DX9", SCREEN_WIDTH, SCREEN_HEIGHT);
	KCDXCreateDevice();

	KCDXMainLoop();

	return 0;
}
