#ifndef KCODE_BOSSKEY_UTIL_H
#define KCODE_BOSSKEY_UTIL_H

#include <Windows.h>
#include <string>

namespace BossKey
{

class ModifierKeyParser
{
public:
	ModifierKeyParser(const std::string &hotkeyStr);
	~ModifierKeyParser();

	bool IsValid();
	UINT GetModifier();
	UINT GetKey();

private:
	ModifierKeyParser(const ModifierKeyParser&);
	ModifierKeyParser& operator=(const ModifierKeyParser&);

	UINT FindF1ToF12(const std::string &s);

private:
	UINT modifier_;
	UINT key_;
	bool valid_;
};

}

#endif