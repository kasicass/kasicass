#ifndef K1R_RENDER_DX9RENDERABLE_OBJECT_H
#define K1R_RENDER_DX9RENDERABLE_OBJECT_H

#include "K1RRenderableObject.h"
#include "K1RDX9Defines.h"

namespace K1R {

struct DX9VertexDescription;

class DX9RenderableObject : public RenderableObject
{
public:
	DX9RenderableObject(VERTEX_TYPE type);
	virtual ~DX9RenderableObject();

	virtual void CreateVertexBuffer(unsigned int n);
	virtual void SetVertex(unsigned int i, void *vertex);
	DX9::VertexBuffer GetVertexBuffer();
	unsigned int GetVertexSize() const;
	unsigned int GetVertexNum() const;

	virtual void CreateIndexBuffer(unsigned int n);
	virtual void SetIndex(unsigned int i, unsigned short t0, unsigned short t1, unsigned short t2);
	DX9::IndexBuffer GetIndexBuffer();
	unsigned int GetIndexNum() const;

	DWORD GetFVF() const;

private:
	DX9VertexDescription& vertexDesc_;
	unsigned int m_VertexNum;
	unsigned int m_IndexNum;

	DX9::VertexBuffer m_pVB;
	DX9::IndexBuffer m_pIB;
};

}

#endif