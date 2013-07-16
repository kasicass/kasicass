#include "BossKeyUtil.h"
#include <ctype.h>
#include <algorithm>

namespace BossKey
{

ModifierKeyParser::ModifierKeyParser(const std::string &hotkeyStr) :
	modifier_(0),
	key_(0),
	valid_(false)
{
	if (hotkeyStr.empty())
		return;

	std::string s(hotkeyStr);
	std::transform(s.begin(), s.end(), s.begin(), toupper);
	if (s.find_last_of('+') == std::string::npos)
		return;

	if (s.find("ALT") != std::string::npos) modifier_ |= MOD_ALT;
	if (s.find("SHIFT") != std::string::npos) modifier_ |= MOD_SHIFT;
	if (s.find("CTRL") != std::string::npos) modifier_ |= MOD_CONTROL;
	if (modifier_ == 0)
		return;

	std::string::size_type n = s.find_last_not_of(' ');
	if (n == std::string::npos)
		return;

	if ('A' <= s[n] && s[n] <= 'Z')
	{
		key_ = s[n];
		valid_ = true;
	}
	else if ((key_ = FindF1ToF12(s)) > 0)
	{
		printf("aa %u\n", key_);
		valid_ = true;
	}
}

ModifierKeyParser::~ModifierKeyParser()
{
}

bool ModifierKeyParser::IsValid()
{
	return valid_;
}

UINT ModifierKeyParser::GetModifier()
{
	return modifier_;
}

UINT ModifierKeyParser::GetKey()
{
	return key_;
}

UINT ModifierKeyParser::FindF1ToF12(const std::string &s)
{
	const char* KeyList[] = {"F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12"};
	for (int i = 11; i >= 0; i--)
	{
		if (s.find(KeyList[i]) != std::string::npos)
			return VK_F1 + i;
	}
	return 0;
}

}
