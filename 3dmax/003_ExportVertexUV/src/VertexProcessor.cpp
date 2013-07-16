#include "VertexProcessor.hpp"

int VertexProcessor::callback(INode *node)
{
	if (!node->Selected())
		return TREE_CONTINUE;
	
	// texture uv
	Object *obj = node->EvalWorldState(t_).obj;
	if (!obj->CanConvertToType(triObjectClassID))
		return TREE_CONTINUE;

	if (!obj->HasUVW())
		return TREE_CONTINUE;

	fprintf(out_, "=============== node: %s ===============\n", node->GetName());

	TriObject *tri = (TriObject *)obj->ConvertToType(t_, triObjectClassID);
	Mesh &mesh = tri->GetMesh();

	int i, n;
	for (n=0; n<mesh.numFaces; n++)
	{
		Face &face = mesh.faces[n];
		TVFace &tvface = mesh.tvFace[n];
		
		fprintf(out_, "triangle #%d\n", n);
		for (i=0; i<3; i++)
		{
			Point3 &point  = mesh.verts[face.v[i]];
			UVVert &uv = mesh.tVerts[tvface.t[i]];
			fprintf(out_, "v%: xyz(%.2f,%.2f,%.2f) uv(%d,%d,%d)\n", point.x, point.y, point.z, uv.x, uv.y, uv.z);
		}
		fprintf(out_, "\n");
	}

	if ((Object *)(tri) != obj)
	{
		tri->DeleteThis();
		tri = NULL;
	}

	// diffuse texture
	Mtl *mtl = node->GetMtl();
	if (mtl && mtl->ClassID() == Class_ID(DMTL_CLASS_ID, 0))
	{
		// StdMtl
		StdMat *stdmat = dynamic_cast<StdMat*>(mtl);
		Texmap *map = nullptr;
		if (stdmat->MapEnabled(ID_DI) && (map = mtl->GetSubTexmap(ID_DI)) != nullptr)
		{
			fprintf(out_, "diffuse tex(id:%s, file:%s)\n", map->GetName(), ((BitmapTex*)map)->GetMapName());
		}
	}

	return TREE_CONTINUE;
}