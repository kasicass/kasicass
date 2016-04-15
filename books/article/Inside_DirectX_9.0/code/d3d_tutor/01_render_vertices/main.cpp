// 绘制基本的一个三角形


#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <tchar.h>
#include <D3D9.h>
#include <stdio.h>


#define	MY_WND_CLASS_NAME		_T("MySuperDxProgram")



#define D3DFVF_CUSTOMVERTEX (D3DFVF_XYZRHW|D3DFVF_DIFFUSE)

struct CUSTOMVERTEX
{
    FLOAT x, y, z, rhw; // The transformed position for the vertex.
    DWORD color;        // The vertex color.
};




HINSTANCE g_hInst;		// current instance
HWND      g_hWnd;

LPDIRECT3D9             g_pD3D = NULL;
LPDIRECT3DDEVICE9       g_pDevice = NULL;
LPDIRECT3DVERTEXBUFFER9 g_pVB = NULL;

CUSTOMVERTEX vertices[] = {
	{ 150.0f,  50.0f, 0.5f, 1.0f, 0xffff0000, },
	{ 250.0f, 250.0f, 0.5f, 1.0f, 0xff00ff00, },
	{  50.0f, 250.0f, 0.5f, 1.0f, 0xff00ffff, },
};


VOID Render()
{
	g_pDevice->Clear(0, NULL, D3DCLEAR_TARGET, D3DCOLOR_XRGB(0,0,255), 1.0f, 0);
	g_pDevice->BeginScene();

	g_pDevice->SetStreamSource(0, g_pVB, 0, sizeof(CUSTOMVERTEX));
	g_pDevice->SetFVF(D3DFVF_CUSTOMVERTEX);
	g_pDevice->DrawPrimitive(D3DPT_TRIANGLELIST, 0, 1);

	g_pDevice->EndScene();
	g_pDevice->Present(NULL, NULL, NULL, NULL);
}

VOID Clearup()
{
	if ( g_pVB != NULL )
		g_pVB->Release();
	if ( g_pDevice != NULL )
		g_pDevice->Release();
	if ( g_pD3D != NULL )
		g_pD3D->Release();
}


LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	switch (message)
	{
	case WM_DESTROY:
		PostQuitMessage(0);
		Clearup();
		break;
	case WM_PAINT:
		Render();
		ValidateRect(hWnd, NULL);
		break;
	default:
		return DefWindowProc(hWnd, message, wParam, lParam);
	}
	return 0;
}


ATOM MyRegisterClass(HINSTANCE hInstance)
{
	WNDCLASSEX wcex;

	wcex.cbSize = sizeof(WNDCLASSEX);

	wcex.style		= CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc	= WndProc;
	wcex.cbClsExtra		= 0;
	wcex.cbWndExtra		= 0;
	wcex.hInstance		= hInstance;
	wcex.hIcon		= NULL;
	wcex.hCursor		= LoadCursor(NULL, IDC_ARROW);
	wcex.hbrBackground	= (HBRUSH)(COLOR_WINDOW+1);
	wcex.lpszMenuName	= NULL;
	wcex.lpszClassName	= MY_WND_CLASS_NAME;
	wcex.hIconSm		= NULL;

	return RegisterClassEx(&wcex);
}


BOOL InitInstance(HINSTANCE hInstance, int nCmdShow)
{
	g_hInst = hInstance; // Store instance handle in our global variable

	g_hWnd = CreateWindow(MY_WND_CLASS_NAME, _T("My Dx9"), WS_OVERLAPPEDWINDOW,
			CW_USEDEFAULT, 0, CW_USEDEFAULT, 0, NULL, NULL, hInstance, NULL);

	if (!g_hWnd)
	{
		return FALSE;
	}

	return TRUE;
}

BOOL CreateDxDevice(void)
{
	D3DPRESENT_PARAMETERS d3dpp;
	
	g_pD3D = Direct3DCreate9(D3D_SDK_VERSION);
	if (NULL == g_pD3D)
	{
		puts("Direct3DCreate9() fail!");
		return FALSE;
	}
	
	ZeroMemory( &d3dpp, sizeof(d3dpp) );
	d3dpp.Windowed   = TRUE;
	d3dpp.SwapEffect = D3DSWAPEFFECT_COPY;
	
	if ( FAILED(g_pD3D->CreateDevice(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, g_hWnd,
		D3DCREATE_SOFTWARE_VERTEXPROCESSING, &d3dpp, &g_pDevice)) )
	{
		puts("Create Device fail!");
		return FALSE;
	}
	
	puts("device ok!");

	if ( FAILED(g_pDevice->CreateVertexBuffer(3*sizeof(CUSTOMVERTEX),
		0, D3DFVF_CUSTOMVERTEX, D3DPOOL_DEFAULT, &g_pVB, NULL)) )
	{
		puts("CreateVertexBuffer() failed!");
		return FALSE;
	}

	puts("vertex buffer ok!");

	VOID *pVertices;
	if ( FAILED(g_pVB->Lock(0, sizeof(vertices), (void**)&pVertices, 0)) )
	{
		puts("vertex buffer lock failed!");
		return FALSE;
	}

	memcpy(pVertices, vertices, sizeof(vertices));
	g_pVB->Unlock();

	return TRUE;
}

int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPTSTR    lpCmdLine,
                     int       nCmdShow)
{
	UNREFERENCED_PARAMETER(hPrevInstance);
	UNREFERENCED_PARAMETER(lpCmdLine);

	MSG msg;
	BOOL bRet;


	MyRegisterClass(hInstance);
	if (!InitInstance (hInstance, nCmdShow))
	{
		return FALSE;
	}

	if (!CreateDxDevice())
	{
		return FALSE;
	}


	ShowWindow(g_hWnd, nCmdShow);
	UpdateWindow(g_hWnd);

	while( (bRet = GetMessage(&msg, NULL, 0, 0)) != 0)
	{ 
		if (bRet == -1)
		{
			// handle the error and possibly exit
			puts("GetMessage() return -1.");
		}
		else
		{
			TranslateMessage(&msg); 
			DispatchMessage(&msg); 
		}
	}
	
	return (int) msg.wParam;
}



int
main(void)
{
	return WinMain( GetModuleHandle(NULL), NULL, GetCommandLine(), SW_NORMAL );
}

