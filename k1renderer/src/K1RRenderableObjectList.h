#ifndef K1R_RENDER_RENDERABLE_OBJECT_LIST_H
#define K1R_RENDER_RENDERABLE_OBJECT_LIST_H

#include "K1RRenderableObject.h"
#include <vector>
#include <memory>

namespace K1R {

class RenderableObjectList
{
public:
	RenderableObjectList();
	~RenderableObjectList();

	void AddRenderableObject(RenderableObjectPtr obj);

	unsigned int RenderableObjectNum() const;
	RenderableObjectPtr GetRenderableObject(unsigned int i) const;

private:
	std::vector<RenderableObjectPtr> m_ObjList;
};
typedef std::shared_ptr<RenderableObjectList> RenderableObjectListPtr;


// global func
RenderableObjectListPtr MakeRenderableObjectList();

}

#endif