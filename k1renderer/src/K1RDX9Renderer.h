#ifndef K1R_RENDERER_DX9RENDERER_H
#define K1R_RENDERER_DX9RENDERER_H

#include "K1RDX9Defines.h"
#include "K1RRenderer.h"
#include "K1RWindowWin32.h"

namespace K1R {

class Window;

class DX9Renderer : public Renderer
{
public:
	DX9Renderer();
	virtual ~DX9Renderer();

	virtual void Initialize(Window *pWin);
	virtual void Draw();

private:
	
	void Release();

	HRESULT InitD3D(HWND hWnd);

private:
	DX9Renderer(DX9Renderer&);
	DX9Renderer& operator= (const DX9Renderer&);

private:
	DX9::DeviceMaker	m_pD3D;
	DX9::Device			m_pD3DDevice;
};


// global func
class Window;
Renderer *MakeRenderer(eRendererType type, Window *pWin);

};

#endif