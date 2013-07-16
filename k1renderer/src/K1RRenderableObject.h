#ifndef K1R_RENDER_RENDERABLE_OBJECT_H
#define K1R_RENDER_RENDERABLE_OBJECT_H

#include "K1RColor.h"
#include <memory>

namespace K1R {

enum VERTEX_TYPE
{
	VERTEX_TYPE_XYZ = 0,
	VERTEX_TYPE_XYZD,
};

struct VERTEX_XYZ
{
	VERTEX_XYZ(float _x, float _y, float _z) : x(_x), y(_y), z(_z) {}
	float x, y, z;
};

struct VERTEX_XYZD
{
	VERTEX_XYZD(float _x, float _y, float _z, Color _d) : x(_x), y(_y), z(_z), diffuse(_d) {} 
	float x, y, z;
	Color diffuse;
};

class RenderableObject
{
public:
	RenderableObject();
	virtual ~RenderableObject();

	virtual void CreateVertexBuffer(unsigned int n) = 0;
	virtual void SetVertex(unsigned int i, void *vertex) = 0;

	virtual void CreateIndexBuffer(unsigned int n) = 0;
	virtual void SetIndex(unsigned int i, unsigned short t0, unsigned short t1, unsigned short t2) = 0;
};

typedef std::shared_ptr<RenderableObject> RenderableObjectPtr;


// global func
RenderableObjectPtr MakeRenderableObject(VERTEX_TYPE type);

}

#endif