#include "VertexProcessor.hpp"

int VertexProcessor::callback(INode *node)
{
	if (!node->Selected())
		return TREE_CONTINUE;

	Object *obj = node->EvalWorldState(t_).obj;
	if (!obj->CanConvertToType(triObjectClassID))
		return TREE_CONTINUE;

	TriObject *tri = (TriObject *)obj->ConvertToType(t_, triObjectClassID);
	Mesh &mesh = tri->GetMesh();
	int numVerts = mesh.getNumVerts();
	for (int i=0; i<numVerts; i++)
	{
		Point3 &point = mesh.getVert(i);
		fprintf(out_, "#%d: %.2f, %.2f, %.2f\n", i, point.x, point.y, point.z);
	}

	if ((Object *)(tri) != obj)
	{
		tri->DeleteThis();
		tri = NULL;
	}

	return TREE_CONTINUE;
}