#include "K1RDX9Renderer.h"
#include "K1RDX9RenderableObject.h"

namespace K1R {

//
// class DX9Renderer
//

DX9Renderer::DX9Renderer()
: m_pD3D(NULL), m_pD3DDevice(NULL)
{
}

DX9Renderer::~DX9Renderer()
{
	Release();
}

void DX9Renderer::Release()
{
	if (m_pD3DDevice != NULL)
		m_pD3DDevice->Release();

	if (m_pD3D != NULL)
		m_pD3D->Release();
}

void DX9Renderer::Initialize(Window *pBaseWin)
{
	WindowWin32 *pWin = dynamic_cast<WindowWin32*>(pBaseWin);
	InitD3D(pWin->GetHWND());
}

HRESULT DX9Renderer::InitD3D(HWND hWnd)
{
    // Create the D3D object.
    if( NULL == ( m_pD3D = Direct3DCreate9( D3D_SDK_VERSION ) ) )
        return E_FAIL;
	
    // Set up the structure used to create the D3DDevice
    D3DPRESENT_PARAMETERS d3dpp;
    ZeroMemory( &d3dpp, sizeof(d3dpp) );
    d3dpp.Windowed = TRUE;
    d3dpp.SwapEffect = D3DSWAPEFFECT_DISCARD;
    d3dpp.BackBufferFormat = D3DFMT_UNKNOWN;

    // Create the D3DDevice
    if( FAILED( m_pD3D->CreateDevice( D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, hWnd,
                                      D3DCREATE_SOFTWARE_VERTEXPROCESSING,
                                      &d3dpp, &m_pD3DDevice ) ) )
    {
        return E_FAIL;
    }

	DX9::SetDevice(m_pD3DDevice);

    // Device state would normally be set here
    return S_OK;
}

void DX9Renderer::Draw()
{
	m_pD3DDevice->Clear(0, NULL, D3DCLEAR_TARGET, D3DCOLOR_XRGB(0,0,255), 1.0f, 0);
	
	m_pD3DDevice->SetRenderState(D3DRS_CULLMODE, D3DCULL_NONE);
	m_pD3DDevice->SetRenderState(D3DRS_LIGHTING, FALSE);
	
	if (SUCCEEDED(m_pD3DDevice->BeginScene()))
	{
		D3DXMATRIX matWorld, matView, matProj;
		
		D3DXMatrixIdentity(&matWorld);
		
		D3DXVECTOR3 vEyePt( 0.0f, 0.0f, -5.0f );
		D3DXVECTOR3 vLookatPt( 0.0f, 0.0f, 0.0f );
		D3DXVECTOR3 vUpVec( 0.0f, 1.0f, 0.0f );
		D3DXMatrixLookAtLH( &matView, &vEyePt, &vLookatPt, &vUpVec );

		float aspectRatio = ((float)800) / ((float)600);
		D3DXMatrixPerspectiveFovLH( &matProj, D3DX_PI/4, aspectRatio, 1.0f, 100.0f );

		m_pD3DDevice->SetTransform(D3DTS_WORLD, &matWorld);
		m_pD3DDevice->SetTransform(D3DTS_VIEW, &matView);
		m_pD3DDevice->SetTransform(D3DTS_PROJECTION, &matProj);

		RenderableObjectListPtr list = this->GetRenderableObjectList();
		for (unsigned int i = 0; i < list->RenderableObjectNum(); i++)
		{
			DX9RenderableObject* obj = reinterpret_cast<DX9RenderableObject*>(list->GetRenderableObject(i).get());
			m_pD3DDevice->SetFVF(obj->GetFVF());

			m_pD3DDevice->SetStreamSource(0, (DX9::VertexBuffer)obj->GetVertexBuffer(), 0, obj->GetVertexSize());
			m_pD3DDevice->SetIndices((DX9::IndexBuffer)obj->GetIndexBuffer());
			m_pD3DDevice->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, obj->GetVertexNum(), 0, obj->GetIndexNum());
		}

		m_pD3DDevice->EndScene();
	}

	m_pD3DDevice->Present(NULL, NULL, NULL, NULL);
}

}