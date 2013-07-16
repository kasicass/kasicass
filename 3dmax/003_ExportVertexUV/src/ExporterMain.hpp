#pragma once

#include "DllMain.hpp"

class KSMeshExporter : public SceneExport
{
public:
    KSMeshExporter() {};
    virtual ~KSMeshExporter() {};

    virtual int ExtCount() { return 1; }
    virtual const TCHAR *Ext(int n) { return (n == 0) ? _T("KSMESH") : _T(""); }

    virtual const TCHAR *LongDesc()   { return _T("KSMesh, LongDesc!"); }
    virtual const TCHAR *ShortDesc()  { return _T("KSMesh, ShortDesc"); }
    virtual const TCHAR *AuthorName() { return _T("kasicass"); }
    virtual const TCHAR *CopyrightMessage() { return _T("nothing!"); }
    virtual const TCHAR *OtherMessage1() { return _T(""); }
    virtual const TCHAR *OtherMessage2() { return _T(""); }
    virtual unsigned int Version() { return 100; }        // v1.00 = 100

    virtual void ShowAbout(HWND hWnd) {}                // optional

    virtual int DoExport(const TCHAR *name, ExpInterface *ei, Interface *i, BOOL suppressPrompts=FALSE, DWORD options=0);
    virtual BOOL SupportsOptions(int ext, DWORD options);
};
