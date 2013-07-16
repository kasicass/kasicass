#include "K1RRenderLib.h"

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

class MyController : public K1R::Controller
{
public:
	MyController() {}
	virtual ~MyController() {}

	virtual void OnKeyDown(K1R::KeyPress key)
	{
		if ( key == K1R::KEY_A )
			::MessageBox(NULL, "KeyDown", "GOD", MB_OK);
	}

	virtual void OnLButtonDown(int x, int y, K1R::KeyState& kState)
	{
		::MessageBox(NULL, "Hello", "Good", MB_OK);
	}
};

int main()
{
	// K1R::Initialize(K1R::DX9_RENDERER);

	K1R::PropList ParamList;
	ParamList.SetInteger("winWidth", 800);
	ParamList.SetInteger("winHeight", 600);
	ParamList.SetString("winTitle", "ÖÐÎÄTitle");

	K1R::Window *pWin = K1R::MakeWindow(ParamList);
	K1R::ControllerPtr ctrl(new MyController());
	pWin->SetController(ctrl);

	K1R::RendererPtr pRenderer = K1R::MakeRenderer(K1R::DX9_RENDERER);
	pWin->SetRenderer(pRenderer);
	pWin->Initialize();

	/*
	K1R::ScenePtr pScene = K1R::MakeScene();
	K1R::TrianglePtr pTriangle = K1R::MakeTriangle();
	pTriangle->SetPoint1(x, y, z, K1R::Color(r,g,b));
	pTriangle->SetPoint2(x, y, z, K1R::Color(r,g,b));
	pTriangle->SetPoint3(x, y, z, K1R::Color(r,g,b));
	pScene->AddObject(pBox);
	pWin->SetScene(pScene);
	*/

	/*
	RenderableObject
	vertex data (position, normal, diffuse, ...)
	index data
	ps/vs
	texture

	K1R::MakeRenderableObject(K1R::VERTEX_TYPE_XYZ);
	VERTEX_XYZ
	VERTEX_XYZD
	VERTEX_XYZDUV
	...
	`

	VertexShaderPtr vs = K1R::MakeVertexShader("shaders/color.vsh");
	PixelShaderPtr ps  = K1R::MakePixelShader("shaders/color.psh");

	pRenderObj->SetVertexShader(vs);
	pRenderObj->SetPixelShader(ps);
	*/

	K1R::RenderableObjectListPtr pRenderObjList = K1R::MakeRenderableObjectList();

	K1R::RenderableObjectPtr pRenderObj = K1R::MakeRenderableObject(K1R::VERTEX_TYPE_XYZD);
	pRenderObj->CreateVertexBuffer(4);
	pRenderObj->SetVertex(0, &K1R::VERTEX_XYZD(-1.0f, -1.0f, 0.0f, K1R::RGBA(255,0,0,255)));
	pRenderObj->SetVertex(1, &K1R::VERTEX_XYZD( 1.0f, -1.0f, 0.0f, K1R::RGBA(0,255,0,255)));
	pRenderObj->SetVertex(2, &K1R::VERTEX_XYZD(-1.0f,  1.0f, 0.0f, K1R::RGBA(0,0,255,255)));
	pRenderObj->SetVertex(3, &K1R::VERTEX_XYZD( 1.0f,  1.0f, 0.0f, K1R::RGBA(255,0,0,255)));

	pRenderObj->CreateIndexBuffer(2);
	pRenderObj->SetIndex(0, 0, 1, 2);
	pRenderObj->SetIndex(1, 2, 1, 3);
	pRenderObjList->AddRenderableObject(pRenderObj);
	
	pRenderObj = K1R::MakeRenderableObject(K1R::VERTEX_TYPE_XYZD);
	pRenderObj->CreateVertexBuffer(3);
	pRenderObj->SetVertex(0, &K1R::VERTEX_XYZD(-1.0f, -1.0f, 0.0f, K1R::RGBA(255,0,0,255)));
	pRenderObj->SetVertex(1, &K1R::VERTEX_XYZD(-1.5f, -0.5f, 0.0f, K1R::RGBA(255,0,0,255)));
	pRenderObj->SetVertex(2, &K1R::VERTEX_XYZD(-1.5f, -1.5f, 0.0f, K1R::RGBA(255,0,0,255)));

	pRenderObj->CreateIndexBuffer(1);
	pRenderObj->SetIndex(0, 0, 1, 2);
	pRenderObjList->AddRenderableObject(pRenderObj);
	
	pRenderer->SetRenderableObjectList(pRenderObjList);

	pWin->Run();
	pWin->Release();

	return 0;
}
