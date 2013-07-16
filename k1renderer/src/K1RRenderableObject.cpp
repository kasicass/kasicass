#include "K1RBase.h"

#if K1R_WIN32
#include "K1RDX9RenderableObject.h"
#endif

namespace K1R {

RenderableObject::RenderableObject()
{}

RenderableObject::~RenderableObject()
{}

RenderableObjectPtr MakeRenderableObject(VERTEX_TYPE type)
{
#if K1R_WIN32
	return RenderableObjectPtr(new DX9RenderableObject(type));
#endif
}

}