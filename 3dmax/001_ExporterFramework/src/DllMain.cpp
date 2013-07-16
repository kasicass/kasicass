#include "DllMain.hpp"
#include "ExportVertex.hpp"

#pragma comment(lib, "core.lib")
#pragma comment(lib, "maxutil.lib")
#pragma comment(lib, "paramblk2.lib")  // ClassDesc2

static HINSTANCE g_hInstance = NULL;

class ExportVertexDesc : public ClassDesc2
{
public:
    virtual int IsPublic() { return TRUE; }
    virtual void* Create(BOOL loading = FALSE) { return new ExportVertex(); }
    virtual const TCHAR* ClassName() { return _T("ExportVertex ClassDesc2::ClassName"); }
    virtual SClass_ID SuperClassID() { return SCENE_EXPORT_CLASS_ID; }
    virtual Class_ID ClassID() { return Class_ID(0x76e74878, 0x6b1b735a); }
    virtual const TCHAR* Category() { return _T("ExportVertex ClassDesc2::Category"); }

    virtual const TCHAR* InternalName() { return _T("ExportVertex ClassDesc2::InternalName"); }
    HINSTANCE HInstance() { return g_hInstance; }
};

static ExportVertexDesc g_exportVertexDesc;

BOOL WINAPI DllMain(HINSTANCE hinstDLL, ULONG fdwReason, LPVOID lpvReserved)
{
    switch (fdwReason)
    {
    case DLL_PROCESS_ATTACH:
        g_hInstance = hinstDLL;
        DisableThreadLibraryCalls(hinstDLL);  // disable DLL_THREAD_ATTACH and DLL_THREAD_DETACH notifications
        break;
    }

    return TRUE;
}


#ifdef __cplusplus
extern "C" {
#endif

KSDLL_EXPORT int LibInitialize(void)
{
    return 1;
}

KSDLL_EXPORT int LibShutdown(void)
{
    return 1;
}

KSDLL_EXPORT const TCHAR* LibDescription()
{
    return _T("Kasicass' 3DMax Plugin!");
}

KSDLL_EXPORT int LibNumberClasses()
{
    return 1;
}

KSDLL_EXPORT ClassDesc* LibClassDesc(int i)
{
    switch (i)
    {
    case 0: return &g_exportVertexDesc;
    default: return NULL;
    }
}

// max用来识别plugin编译时SDK版本的
KSDLL_EXPORT ULONG LibVersion()
{
    return VERSION_3DSMAX;
}

#ifdef __cplusplus
}
#endif