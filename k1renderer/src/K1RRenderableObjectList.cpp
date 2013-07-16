#include "K1RRenderableObjectList.h"

namespace K1R {

RenderableObjectList::RenderableObjectList()
{}

RenderableObjectList::~RenderableObjectList()
{}

void RenderableObjectList::AddRenderableObject(RenderableObjectPtr obj)
{
	m_ObjList.push_back(obj);
}

unsigned int RenderableObjectList::RenderableObjectNum() const
{
	return static_cast<unsigned int>(m_ObjList.size());
}

RenderableObjectPtr RenderableObjectList::GetRenderableObject(unsigned int i) const
{
	return m_ObjList[i];
}



// global func
RenderableObjectListPtr MakeRenderableObjectList()
{
	return RenderableObjectListPtr(new RenderableObjectList);
}

}