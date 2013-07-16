#include "BossKey.h"
#include <stdio.h>

int main()
{
	BossKey::Init("MyPuppy");

	BossKey::AddWindowClsName("HOTKEY");
	BossKey::RegisterHotkey("Shift + D");
	BossKey::RegisterHotkey("Ctrl + Shift + F10");

	MSG msg;
	while (GetMessage(&msg, (HWND)-1, 0, 0))
	{
		if (BossKey::Process(&msg))
			continue;
	}

	BossKey::Shutdown();
	return 0;
}