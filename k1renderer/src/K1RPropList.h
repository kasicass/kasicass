#ifndef K1R_UTIL_PROPLIST_H
#define K1R_UTIL_PROPLIST_H

#include "K1RBase.h"

#include <map>
#include <string>

namespace K1R {

class PropList
{
public:
	PropList() {};
	~PropList() {};

	void SetInteger(const std::string& sKey, const int iVal);
	void SetFloat(const std::string& sKey, const float fVal);
	void SetString(const std::string& sKey, const std::string& sVal);

	int GetInteger(const std::string& sKey, const int iDefaultVal = 0);
	float GetFloat(const std::string& sKey, const float fDefaultVal = 0.0f);
    std::string GetString(const std::string& sKey, const std::string& sDefaultVal = "");

private:
	PropList(const PropList&);
	PropList& operator=(const PropList&);

private:
	std::map<std::string, int> m_IntMap;
	std::map<std::string, float> m_FloatMap;
	std::map<std::string, std::string> m_StrMap;
};																																																																																							

};

#endif