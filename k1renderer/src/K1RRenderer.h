#ifndef K1R_RENDERER_RENDERER_H
#define K1R_RENDERER_RENDERER_H

#include "K1RRenderableObjectList.h"
#include <memory>

namespace K1R {

enum eRendererType {
	DX9_RENDERER,
	SOFTWARE_RENDERER,
	NUM_RENDERERS,
};

class Window;

// base class, interface definition of renderer system
class Renderer
{
public:
	Renderer();
	virtual ~Renderer();

	virtual void Initialize(Window *pWin) = 0;
	virtual void Draw() = 0;

	void SetRenderableObjectList(RenderableObjectListPtr list);
	RenderableObjectListPtr GetRenderableObjectList();

private:
	Renderer(Renderer&);
	Renderer& operator= (const Renderer&);

private:
	RenderableObjectListPtr m_RenderList;
};
typedef std::shared_ptr<Renderer> RendererPtr;


// global func
RendererPtr MakeRenderer(eRendererType type);

};

#endif