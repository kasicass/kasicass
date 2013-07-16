#include "BossKey.h"
#include "BossKeyUtil.h"
#include <set>
#include <map>
#include <string>

namespace BossKey
{

typedef std::set<std::string> CLSNAME_SET;
typedef std::map<ATOM, std::string> HOTKEY_MAP;

static CLSNAME_SET s_clsNameSet;  // clsName of windows to hide
static HOTKEY_MAP s_HotkeyMap;    // hotkeyString => hotkeyID
static std::string s_productName("MyProduct");
static bool s_isShow = true;


void Init(const char *productName)
{
	s_productName = productName;
	s_clsNameSet.clear();
	s_HotkeyMap.clear();
}

void Shutdown()
{
	HOTKEY_MAP::const_iterator it = s_HotkeyMap.begin();
	for (; it != s_HotkeyMap.end(); ++it)
	{
		GlobalDeleteAtom(it->first);
	}

	s_productName = "MyProduct";
	s_clsNameSet.clear();
	s_HotkeyMap.clear();
}

void AddWindowClsName(const char *clsName)
{
	s_clsNameSet.insert(clsName);
}

void RemoveWindowClsName(const char *clsName)
{
	s_clsNameSet.erase(clsName);
}

bool RegisterHotkey(const char *hotkey)
{
	ModifierKeyParser parser(hotkey);
	if (!parser.IsValid())
		return false;

	ATOM hotkeyID = GlobalAddAtomA((s_productName + hotkey).c_str());
	if (s_HotkeyMap.find(hotkeyID) != s_HotkeyMap.end())
		return true;

	if (!RegisterHotKey(NULL, hotkeyID, parser.GetModifier(), parser.GetKey()))
		return false;

	s_HotkeyMap[hotkeyID] = hotkey;
	return true;
}

void UnregisterHotkey(const char *hotkey)
{
	ATOM hotkeyID = GlobalAddAtomA((s_productName + hotkey).c_str());
}

BOOL CALLBACK ToggleMyWindow(HWND hWnd, LPARAM lParam)
{
	char buf[256];
	GetClassNameA(hWnd, buf, 256);
	std::string name(buf);
	if (s_clsNameSet.find(name) != s_clsNameSet.end())
	{
		ShowWindow(hWnd, s_isShow ? SW_SHOW : SW_HIDE);
	}
	return TRUE;
}

static void ToggleWindowsVisibility()
{
	s_isShow = !s_isShow;
	EnumWindows(ToggleMyWindow, NULL);
}

bool Process(MSG *msg)
{
	if (msg->hwnd != NULL)
		return false;
	
	if (msg->message != WM_HOTKEY)
		return false;

	if (s_HotkeyMap.find(msg->wParam) == s_HotkeyMap.end())
		return false;

	ToggleWindowsVisibility();
	return true;
}

}