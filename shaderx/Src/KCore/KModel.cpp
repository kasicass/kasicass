#include "KModel.h"
#include "KCoreDX.h"
#include <string>
#include <vector>

struct KModel
{
	IDirect3DVertexDeclaration9* decl;
	IDirect3DVertexBuffer9* vb;
	IDirect3DIndexBuffer9*  ib;
	unsigned int vertexCount;
	unsigned int faceCount;
};

struct KVertex
{
	KVertex(float _x, float _y, float _z) : x(_x), y(_y), z(_z) {}

	float x, y, z;
};

struct KFace
{
	KFace(int _i0, int _i1, int _i2) : i0(_i0), i1(_i1), i2(_i2) {}

	unsigned short i0, i1, i2;
};

D3DVERTEXELEMENT9 DeclElements[] = {
	{0, 0, D3DDECLTYPE_FLOAT3, D3DDECLMETHOD_DEFAULT, D3DDECLUSAGE_POSITION, 0},
	D3DDECL_END(),
};

static std::string KReadLine(FILE *fp)
{
	std::string line;
	while (1)
	{
		int c = fgetc(fp);
		if (c == EOF)
		{
			line = "end_of_file";
			break;
		}

		if (c == '\r')
		{
			int c1 = fgetc(fp);
			if (c1 != '\n') ungetc(c1, fp);
			break;
		}

		if (c == '\n')
			break;

		line.push_back(c);
	}

	return line;
}

struct KModel *KLoadModel(const std::string &path)
{
	// get data from file
	FILE *fp = fopen(path.c_str(), "r");
	std::string line;

	std::vector<KVertex> vertices;
	std::vector<KFace> indices;

	while ((line = KReadLine(fp)) != "end_of_file")
	{
		if (line.empty())
			continue;
		
		switch (line[0])
		{
		case 'v':
			{
			float x, y, z;
			if (sscanf(line.c_str(), "v %f %f %f", &x, &y, &z) == 3)
			{
				vertices.push_back(KVertex(x,y,z));
			}
			}
			break;

		case 'f':
			{
			int i0, i1, i2;
			if (sscanf(line.c_str(), "f %d %d %d", &i0, &i1, &i2) == 3)
			{
				indices.push_back(KFace(i0-1, i1-1, i2-1));
			}
			}
			break;
		}
	}
	fclose(fp);

	// make KModel
	struct KModel *pModel = (struct KModel *)malloc(sizeof(struct KModel));
	memset(pModel, 0, sizeof(struct KModel));

	IDirect3DDevice9* pd3dDevice = KCDXGetDevice();
	HRESULT hr = pd3dDevice->CreateVertexDeclaration(DeclElements, &pModel->decl);
	KCDX_HR_FAILCHECK(hr, "KModel, pd3dDevice->CreateVertexDeclaration");

	size_t i;
	pd3dDevice->CreateVertexBuffer(vertices.size()*sizeof(KVertex), D3DUSAGE_WRITEONLY, 0, D3DPOOL_MANAGED, &pModel->vb, NULL);
	VOID* pVertices;
	pModel->vb->Lock(0, vertices.size()*sizeof(KVertex), &pVertices, 0);
	for (i = 0; i < vertices.size(); i++)
	{
		*((KVertex*)pVertices + i) = vertices[i];
	}
	pModel->vb->Unlock();

	pd3dDevice->CreateIndexBuffer(indices.size()*sizeof(KFace), 0, D3DFMT_INDEX16, D3DPOOL_MANAGED, &pModel->ib, NULL);
	VOID *pIndices;
	pModel->ib->Lock(0, indices.size()*sizeof(KFace), &pIndices, 0);
	for (i = 0; i < indices.size(); i++)
	{
		*((KFace*)pIndices + i) = indices[i];
	}
	pModel->ib->Unlock();

	pModel->vertexCount = vertices.size();
	pModel->faceCount   = indices.size();
	return pModel;
}

void KDrawModel(struct KModel *obj)
{
	IDirect3DDevice9* pd3dDevice = KCDXGetDevice();
	pd3dDevice->SetVertexDeclaration(obj->decl);

	pd3dDevice->SetStreamSource(0, obj->vb, 0, sizeof(KVertex));
	pd3dDevice->SetIndices(obj->ib);
	pd3dDevice->DrawIndexedPrimitive(D3DPT_TRIANGLELIST, 0, 0, obj->vertexCount, 0, obj->faceCount);
}

void KDestroyModel(struct KModel *obj)
{
	KSAFE_RELEASE(obj->decl);
	KSAFE_RELEASE(obj->vb);
	KSAFE_RELEASE(obj->ib);
	free(obj);
}
