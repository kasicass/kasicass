#include "K1RRenderer.h"
#include "K1RDX9Renderer.h"

namespace K1R {

//
// class Renderer
//

Renderer::Renderer()
{ }

Renderer::~Renderer()
{ }

void Renderer::SetRenderableObjectList(RenderableObjectListPtr list)
{
	m_RenderList = list;
}

RenderableObjectListPtr Renderer::GetRenderableObjectList()
{
	return m_RenderList;
}


// global func
RendererPtr MakeRenderer(eRendererType type)
{
	return RendererPtr(new DX9Renderer());
}

}