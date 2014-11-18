// =============================================================
// 
//  Open Game Engine Exchange
//  http://opengex.org/
// 
//  Export plugin for 3D Studio Max
//  by Eric Lengyel
// 
//  Copyright 2014, Terathon Software LLC
// 
//  This software is licensed under the Creative Commons
//  Attribution-ShareAlike 3.0 Unported License:
// 
//  http://creativecommons.org/licenses/by-sa/3.0/deed.en_US
// 
// =============================================================


#ifndef OpenGex_Max_h
#define OpenGex_Max_h

#include <max.h>
#include <iparamb2.h>
#include <modstack.h>
#include <stdmat.h>
#include <iskin.h>
#include <CS/bipexp.h>

/*
#include "maxapi.h"
#include "units.h"
#include "inode.h"
#include "triobj.h"
#include "iskin.h"
#include "genlight.h"
#include "control.h"
#include "modstack.h"
#include "stdmat.h"
#include "istdplug.h"
#include "iparamb2.h"
#include "impexp.h"
#include "CS/bipexp.h"
#include "VertexNormal.h"
#include "euler.h"
*/

#include <vector>
#include <sstream>


class OpenGexClassDesc : public ClassDesc2
{
	public:

		OpenGexClassDesc();
		~OpenGexClassDesc();

		int IsPublic(void) override;
		void *Create(BOOL loading) override;
		const MCHAR *ClassName(void) override;
		SClass_ID SuperClassID(void) override;
		Class_ID ClassID(void) override;
		const MCHAR *Category(void) override;
};


namespace OpenGex
{
	enum
	{
		kMaxTexcoordCount = 2
	};


	enum
	{
		kNodeTypeNode,
		kNodeTypeBone,
		kNodeTypeGeometry,
		kNodeTypeLight,
		kNodeTypeCamera,
		kNodeTypeCount
	};


	struct NodeReference
	{
		INode			*node;
		int				nodeType;
		std::string		structName;

		NodeReference(INode *n, int type, const char *name) : node(n), nodeType(type), structName(name) {}
		NodeReference(const NodeReference& nodeReference) : node(nodeReference.node), nodeType(nodeReference.nodeType), structName(nodeReference.structName) {}
	};


	struct ObjectReference
	{
		Object					*object;
		std::string				structName;
		std::vector<INode *>	nodeTable;

		ObjectReference(Object *obj, const char *name, INode *node) : object(obj), structName(name), nodeTable(1, node) {}
		ObjectReference(const ObjectReference& objectReference) : object(objectReference.object), structName(objectReference.structName), nodeTable(objectReference.nodeTable) {}
	};


	struct MaterialReference
	{
		StdMat					*material;
		std::string				structName;

		MaterialReference(StdMat *mat, const char *name) : material(mat), structName(name) {}
		MaterialReference(const MaterialReference& materialReference) : material(materialReference.material), structName(materialReference.structName) {}
	};


	struct ExportVertex
	{
		unsigned int	hash;
		unsigned int	index;

		Point3			position;
		Point3			normal;
		Point3			color;
		Point2			texcoord[kMaxTexcoordCount];

		ExportVertex();

		bool operator ==(const ExportVertex& v) const;

		void Hash(void);
	};
}


class OpenGexExport : public SceneExport
{
	private:

		HANDLE			exportFile;
		int				indentLevel;

		TimeValue		startTime;
		TimeValue		endTime;

		std::vector<OpenGex::NodeReference>			*nodeArray;
		std::vector<OpenGex::ObjectReference>		*geometryArray;
		std::vector<OpenGex::ObjectReference>		*lightArray;
		std::vector<OpenGex::ObjectReference>		*cameraArray;
		std::vector<OpenGex::MaterialReference>		*materialArray;

		void Write(const void *buffer, unsigned int size) const;
		void Write(const char *string) const;
		void Write(const wchar_t *string) const;
		void IndentWrite(const char *string, int extra = 0, bool newline = false) const;

		void WriteInt(int i) const;
		void WriteUnsignedInt(unsigned int i) const;
		void WriteFloat(float f) const;
		void WriteHexFloat(float f) const;
		void WriteMatrix(const Matrix3& matrix) const;
		void WriteMatrixFlat(const Matrix3& matrix) const;
		void WriteHexMatrixFlat(const Matrix3& matrix) const;
		void WritePoint3(const Point3& point) const;
		void WriteHexPoint3(const Point3& point) const;
		void WriteQuat(const Quat& quat) const;
		void WriteHexQuat(const Quat& quat) const;
		void WriteColor(const Color& color) const;
		void WriteFileName(const char *string) const;

		void WriteIntArray(int count, const int *value) const;
		void WriteFloatArray(int count, const float *value) const;

		void WriteVertex(const Point2& vertex) const;
		void WriteVertex(const Point3& vertex) const;
		template <class type> void WriteVertexArray(int count, const type *vertex, int stride) const;

		void WriteTriangle(int triangleIndex, const int *indexTable) const;
		void WriteTriangleArray(int count, const int *indexTable) const;

		void WriteNodeTable(const OpenGex::ObjectReference *objectRef) const;

		int GetNodeType(INode *node) const;
		static ISkin *GetSkinInterface(Object *object);

		OpenGex::NodeReference *FindNode(const INode *node) const;
		static OpenGex::ObjectReference *FindObject(std::vector<OpenGex::ObjectReference> *array, const Object *object);
		OpenGex::MaterialReference *FindMaterial(const StdMat *material);

		static OpenGex::ExportVertex *DeindexMesh(Mesh *mesh, Mesh *baseMesh, int *exportTriangleCount, int *exportColorCount, int *exportTexcoordCount);
		static int FindExportVertex(const std::vector<int>& bucket, const OpenGex::ExportVertex *exportVertex, const OpenGex::ExportVertex& vertex);
		static int UnifyVertices(int vertexCount, const OpenGex::ExportVertex *exportVertex, OpenGex::ExportVertex *unifiedVertex, int *indexTable);

		void ProcessNode(INode *node);
		void ProcessObjects(void);

		template <class keyType> static bool AnimationKeysDifferent(IKeyControl *keyInterface);
		template <class keyType> static bool AnimationTangentsNonzero(IKeyControl *keyInterface);
		static bool AnimationPresent(Control *control);

		template <class keyType> void ExportKeyTimes(IKeyControl *keyInterface);
		template <class keyType> void ExportKeyTimeControlPoints(IKeyControl *keyInterface);
		void ExportFloatKeyTimeControlPoints(IKeyControl *keyInterface);
		template <class keyType> void ExportFloatKeyValues(IKeyControl *keyInterface);
		void ExportFloatKeyValueControlPoints(IKeyControl *keyInterface);
		template <class keyType> void ExportPoint3KeyValues(IKeyControl *keyInterface);
		template <class keyType> void ExportPoint3KeyValueControlPoints(IKeyControl *keyInterface);
		template <class keyType> void ExportQuatKeyValues(IKeyControl *keyInterface, bool relative = false);
		template <class keyType> void ExportInvQuatKeyValues(IKeyControl *keyInterface);
		template <class keyType> void ExportFloatKeyData(IKeyControl *keyInterface, const char *kind, const float keyType::*data);

		void ExportAnimationTrack(Control *control, const char *target, bool newline);
		void ExportSampledAnimationTrack(INode *node);

		void ExportObjectTransform(INode *node);
		void ExportTransform(INode *node);

		void ExportNode(INode *node);
		void ExportMaterialRef(Mtl *material, int index = -1);

		void ExportSkin(ISkin *skin, INode *node, int vertexCount, const OpenGex::ExportVertex *exportVertex);
		void ExportGeometry(const OpenGex::ObjectReference *objectRef);
		void ExportLight(const OpenGex::ObjectReference *objectRef);
		void ExportCamera(const OpenGex::ObjectReference *objectRef);
		void ExportObjects(void);

		void ExportTexture(StdMat *material, int slot, const char *attrib);
		void ExportMaterials(void);

		void ExportMetrics(void);

	public:

		OpenGexExport();
		~OpenGexExport();

		int ExtCount(void) override;
		const MCHAR *Ext(int n) override;

		const MCHAR *LongDesc(void) override;
		const MCHAR *ShortDesc(void) override;

		const MCHAR *AuthorName(void) override;
		const MCHAR *CopyrightMessage(void) override;
		const MCHAR *OtherMessage1(void) override;
		const MCHAR *OtherMessage2(void) override;

		unsigned int Version(void) override;
		void ShowAbout(HWND hWnd) override;

		int DoExport(const MCHAR *name, ExpInterface *ei, Interface *i, BOOL suppressPrompts, DWORD options) override;
};


#endif
