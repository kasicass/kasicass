#ifndef KCODE_BOSSKEY_H
#define KCODE_BOSSKEY_H

// ==== Usage ====
// BossKey::Init("YourProduct");
// BossKey::AddWindowClsName("OneWinCls");
// BossKey::AddWindowClsName("AnotherWinCls");
// BossKey::RegisterHotkey("Shift + D");
// BossKey::RegisterHotkey("Ctrl + Shift + E");
// BossKey::RegisterHotkey("Ctrl + F10");
// 
// while (::PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
// {
//   if (BossKey::Process(&msg))
//     continue;
//
//   ...
// }
//
// BossKey::Shutdown();

#include <Windows.h>

// single-thread support only
namespace BossKey
{
	void Init(const char *productName);
	void Shutdown();

	void AddWindowClsName(const char *clsName);
	void RemoveWindowClsName(const char *clsName);

	// Ctrl, Shift, Alt, A/B/...
	bool RegisterHotkey(const char *hotkey);
	void UnregisterHotkey(const char *hotkey);

	bool Process(MSG *msg);
}

#endif