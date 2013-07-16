#include "ExportVertex.hpp"

// 导出场景数据
// name, filename
// ei, 用于遍历场景对象
// i, 用于调用3dmax的一些接口
// options, 同SupportsOptions
// 返回值, IMPEXP_FAIL/IMPEXP_SUCCESS/IMPEXP_CANCEL
int ExportVertex::DoExport(const TCHAR *name, ExpInterface *ei, Interface *i, BOOL suppressPrompts, DWORD options)
{
    ::MessageBox(NULL, name, _T("Export it!"), MB_OK);
    return IMPEXP_SUCCESS;
}

// 检测Plugin是否支持某个特性
// options, 目前只有 SCENE_EXPORT_SELECTED(导出选中物体) 这么一个选项
BOOL ExportVertex::SupportsOptions(int ext, DWORD options)
{
    if (ext == 0 && options == SCENE_EXPORT_SELECTED)
        return TRUE;

    return FALSE;
}
