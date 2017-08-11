/*------------------------------------------------------------
   HELLOWIN.C -- Displays "Hello, Windows 98!" in client area
                 (c) Charles Petzold, 1998
  ------------------------------------------------------------*/

#include <windows.h>
#include <windowsx.h>
#include <stdio.h>
#include <string.h>

static HHOOK s_mouseHookLowLevel;
static HHOOK s_keyboardHookLowLevel;

static HHOOK s_mouseHook;
static HHOOK s_keyboardHook;

static HHOOK s_wndProcHook;

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

void DbgViewPrintf(const char* format, ...)
{
	const unsigned int bufsz = 1024;
    static char buf[bufsz];

    unsigned int n;

	va_list args;
	va_start(args, format);
	n = vsnprintf(buf, bufsz-1, format, args);
	va_end(args);

	buf[n] = 0;

    OutputDebugStringA(buf);
}

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("HelloWin") ;
     HWND         hwnd ;
     MSG          msg ;
     WNDCLASS     wndclass ;

     wndclass.style         = CS_HREDRAW | CS_VREDRAW ;
     wndclass.lpfnWndProc   = WndProc ;
     wndclass.cbClsExtra    = 0 ;
     wndclass.cbWndExtra    = 0 ;
     wndclass.hInstance     = hInstance ;
     wndclass.hIcon         = LoadIcon (NULL, IDI_APPLICATION) ;
     wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
     wndclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH) ;
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;

     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"), 
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName,                  // window class name
                          TEXT ("The Hello Program"), // window caption
                          WS_OVERLAPPEDWINDOW,        // window style
                          CW_USEDEFAULT,              // initial x position
                          CW_USEDEFAULT,              // initial y position
                          CW_USEDEFAULT,              // initial x size
                          CW_USEDEFAULT,              // initial y size
                          NULL,                       // parent window handle
                          NULL,                       // window menu handle
                          hInstance,                  // program instance handle
                          NULL) ;                     // creation parameters
     
     ShowWindow (hwnd, iCmdShow) ;
     UpdateWindow (hwnd) ;
     
     while (GetMessage (&msg, NULL, 0, 0))
     {
          TranslateMessage (&msg) ;
          DispatchMessage (&msg) ;
     }
     return msg.wParam ;
}

static char* MouseMessageName(WPARAM wParam)
{
    switch (wParam)
    {
    case WM_LBUTTONDOWN: return "WM_LBUTTONDOWN";
    case WM_LBUTTONUP: return "WM_LBUTTONUP";
    case WM_MOUSEMOVE: return "WM_MOUSEMOVE";
    case WM_MOUSEWHEEL: return "WM_MOUSEWHEEL";
    case WM_MOUSEHWHEEL: return "WM_MOUSEHWHEEL";
    case WM_RBUTTONDOWN: return "WM_RBUTTONDOWN";
    case WM_RBUTTONUP: return "WM_RBUTTONUP";
    default: return "None";
    }
}

static char* KeyboardMessageName(WPARAM wParam)
{
    switch (wParam)
    {
    case WM_KEYDOWN: return "WM_KEYDOWN";
    case WM_KEYUP: return "WM_KEYUP";
    case WM_SYSKEYDOWN: return "WM_SYSKEYDOWN";
    case WM_SYSKEYUP: return "WM_SYSKEYUP";
    default: return "None";
    }
}

static bool s_activeWnd = false;
static LRESULT CALLBACK mouse_ll( int nCode, WPARAM wParam, LPARAM lParam )
{
    if (nCode < 0 || !s_activeWnd)
        return CallNextHookEx(s_mouseHookLowLevel, nCode, wParam, lParam);
    
    MSLLHOOKSTRUCT *s = (MSLLHOOKSTRUCT*)lParam;
    if (s->flags & LLMHF_INJECTED)
    {
        DbgViewPrintf("kasicass xx mouse_ll, %s, (%d,%d)\n", MouseMessageName(wParam), s->pt.x, s->pt.y);
        return 1;  // eat me
    }
    else
    {
        DbgViewPrintf("kasicass xx mouse_ll 2, %s\n", MouseMessageName(wParam));
    }

    return CallNextHookEx(s_mouseHookLowLevel, nCode, wParam, lParam);
}

static LRESULT CALLBACK keyboard_ll( int nCode, WPARAM wParam, LPARAM lParam )
{
    if (nCode < 0 || !s_activeWnd)
        return CallNextHookEx(s_keyboardHookLowLevel, nCode, wParam, lParam);
    
    KBDLLHOOKSTRUCT *s = (KBDLLHOOKSTRUCT*)lParam;
	if (s->flags & LLKHF_INJECTED)
    {
        DbgViewPrintf("kasicass xx keyboard_ll, %s, %c\n", KeyboardMessageName(wParam), s->vkCode);
        return 1;  // eat me
    }
    else
    {
        DbgViewPrintf("kasicass xx keyboard_ll 2, %s, %c\n", KeyboardMessageName(wParam), s->vkCode);
    }

    return CallNextHookEx(s_keyboardHookLowLevel, nCode, wParam, lParam);
}

static LRESULT CALLBACK mouse_2( int nCode, WPARAM wParam, LPARAM lParam )
{
    if (nCode < 0 || !s_activeWnd)
        return CallNextHookEx(s_mouseHook, nCode, wParam, lParam);
    
    DbgViewPrintf("kasicass xx mouse, %s\n", MouseMessageName(wParam));

    return CallNextHookEx(s_mouseHook, nCode, wParam, lParam);
}

static LRESULT CALLBACK keyboard_2( int nCode, WPARAM wParam, LPARAM lParam )
{
    if (nCode < 0 || !s_activeWnd)
        return CallNextHookEx(s_keyboardHook, nCode, wParam, lParam);
    
    DbgViewPrintf("kasicass xx keyboard, %c\n", wParam);

    return CallNextHookEx(s_keyboardHook, nCode, wParam, lParam);
}

static LRESULT CALLBACK wndproc_hook(int nCode, WPARAM wParam, LPARAM lParam)
{
    if (nCode < 0 || !s_activeWnd)
        return CallNextHookEx(s_wndProcHook, nCode, wParam, lParam);

    if (wParam == 0)
    {
        CWPSTRUCT *s = (CWPSTRUCT*)lParam;
        DbgViewPrintf("kasicass xx wndproc, msg:0x%x\n", s->message);
        /*
        if (s->message == WM_KEYDOWN)
        {
            DbgViewPrintf("kasicass xx wndproc, WM_KEYDOWN, threadId:%u, char:%c\n", wParam, s->wParam);
        }
        else
        {
            DbgViewPrintf("kasicass xx wndproc, threadId:%u\n", wParam);
        }
        */
    }

    return CallNextHookEx(s_wndProcHook, nCode, wParam, lParam);
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    static int xPos = 1, yPos = 1;
    static char msgBuf[256] = {0};
    
    HDC         hdc ;
    PAINTSTRUCT ps ;
    RECT        rect ;
     
    switch (message)
    {
    case WM_CREATE:
        //s_mouseHookLowLevel = SetWindowsHookEx(WH_MOUSE_LL, (HOOKPROC)mouse_ll, GetModuleHandle(0), 0);
        //s_keyboardHookLowLevel = SetWindowsHookEx(WH_KEYBOARD_LL, (HOOKPROC)keyboard_ll, GetModuleHandle(0), 0);

        s_mouseHook = SetWindowsHookEx(WH_MOUSE, (HOOKPROC)mouse_2, GetModuleHandle(0), GetCurrentThreadId());
        s_keyboardHook = SetWindowsHookEx(WH_KEYBOARD, (HOOKPROC)keyboard_2, GetModuleHandle(0), GetCurrentThreadId());
        
        // s_wndProcHook = SetWindowsHookEx(WH_CALLWNDPROC, (HOOKPROC)wndproc_hook, GetModuleHandle(0), GetCurrentThreadId());
        return 0;

    case WM_ACTIVATEAPP:
        s_activeWnd = !!wParam;
        s_activeWnd = true;
        return 0;

    case WM_LBUTTONDOWN:
        xPos = GET_X_LPARAM(lParam); 
        yPos = GET_Y_LPARAM(lParam);
        sprintf(msgBuf, "WM_LMOUSEDOWN, X:%d, Y:%d", xPos, yPos);

        InvalidateRect(hwnd, NULL, TRUE);
        UpdateWindow(hwnd);
        return 0;

    case WM_PAINT:
		hdc = BeginPaint (hwnd, &ps) ;
        
        GetClientRect (hwnd, &rect) ;
        
        if (strlen(msgBuf) > 0)
        {
            DrawText (hdc,msgBuf, -1, &rect,
                    DT_SINGLELINE | DT_CENTER | DT_VCENTER) ;
        }
        else
        {
            DrawText (hdc, TEXT ("Hello, Windows 98!"), -1, &rect,
                    DT_SINGLELINE | DT_CENTER | DT_VCENTER) ;
        }
        
        EndPaint (hwnd, &ps) ;
        return 0 ;
          
    case WM_DESTROY:
        PostQuitMessage (0) ;
        return 0 ;
    }
    return DefWindowProc (hwnd, message, wParam, lParam) ;
}