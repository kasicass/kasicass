#include "K1RDX9RenderableObject.h"
#include <assert.h>

namespace K1R {

struct DX9VertexDescription
{
	unsigned int vertexSize;
	DWORD fvf;
} s_VertexDesc[] = {
	{sizeof(struct VERTEX_XYZ),  (D3DFVF_XYZ)},
	{sizeof(struct VERTEX_XYZD), (D3DFVF_XYZ|D3DFVF_DIFFUSE)},
};

DX9RenderableObject::DX9RenderableObject(VERTEX_TYPE type) :
	vertexDesc_(s_VertexDesc[type]),
	m_VertexNum(0),
	m_IndexNum(0),
	m_pVB(NULL),
	m_pIB(NULL)
{
}

DX9RenderableObject::~DX9RenderableObject()
{
	DX9::SafeRelease(m_pVB);
	DX9::SafeRelease(m_pIB);
}

void DX9RenderableObject::CreateVertexBuffer(unsigned int n)
{
	DX9::SafeRelease(m_pVB);

	m_VertexNum = n;
	DX9::Device pd3dDevice = DX9::GetDevice();
	pd3dDevice->CreateVertexBuffer(n*vertexDesc_.vertexSize, D3DUSAGE_WRITEONLY, 0, D3DPOOL_MANAGED, &m_pVB, NULL);
	assert(m_pVB != NULL);
}

void DX9RenderableObject::SetVertex(unsigned int i, void *vertex)
{
	VOID* pVertices;
	m_pVB->Lock(i*vertexDesc_.vertexSize, vertexDesc_.vertexSize, &pVertices, 0);
	memcpy(pVertices, vertex, vertexDesc_.vertexSize);
	m_pVB->Unlock();
}

DX9::VertexBuffer DX9RenderableObject::GetVertexBuffer()
{
	return m_pVB;
}

unsigned int DX9RenderableObject::GetVertexSize() const
{
	return vertexDesc_.vertexSize;
}

unsigned int DX9RenderableObject::GetVertexNum() const
{
	return m_VertexNum;
}

void DX9RenderableObject::CreateIndexBuffer(unsigned int n)
{
	DX9::SafeRelease(m_pIB);

	m_IndexNum = n;
	DX9::Device pd3dDevice = DX9::GetDevice();
	pd3dDevice->CreateIndexBuffer(n*3*sizeof(unsigned short), 0, D3DFMT_INDEX16, D3DPOOL_MANAGED, &m_pIB, NULL);
	assert(m_pIB != NULL);
}

void DX9RenderableObject::SetIndex(unsigned int i, unsigned short t0, unsigned short t1, unsigned short t2)
{
	VOID *pIndices;
	m_pIB->Lock(i*3*sizeof(unsigned short), 3*sizeof(unsigned short), &pIndices, 0);
	((unsigned short*)pIndices)[0] = t0;
	((unsigned short*)pIndices)[1] = t1;
	((unsigned short*)pIndices)[2] = t2;
	m_pIB->Unlock();
}

DX9::IndexBuffer DX9RenderableObject::GetIndexBuffer()
{
	return m_pIB;
}

unsigned int DX9RenderableObject::GetIndexNum() const
{
	return m_IndexNum;
}

DWORD DX9RenderableObject::GetFVF() const
{
	return vertexDesc_.fvf;
}

}