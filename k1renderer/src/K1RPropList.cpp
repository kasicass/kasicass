#include "K1RPropList.h"

namespace K1R {

void PropList::SetInteger(const std::string& sKey, const int iVal)
{
	m_IntMap[sKey] = iVal;
}

void PropList::SetFloat(const std::string& sKey, const float fVal)
{
	m_FloatMap[sKey] = fVal;
}

void PropList::SetString(const std::string& sKey, const std::string& sVal)
{
	m_StrMap[sKey] = sVal;
}

int PropList::GetInteger(const std::string& sKey, const int iDefaultVal)
{
	if ( m_IntMap.find(sKey) != m_IntMap.end() )
		return m_IntMap[sKey];
	else
		return iDefaultVal;
}

float PropList::GetFloat(const std::string& sKey, const float fDefaultVal)
{
	if ( m_FloatMap.find(sKey) != m_FloatMap.end() )
		return m_FloatMap[sKey];
	else
		return fDefaultVal;
}

std::string PropList::GetString(const std::string& sKey, const std::string& sDefaultVal)
{
	if ( m_StrMap.find(sKey) != m_StrMap.end() )
		return m_StrMap[sKey];
	else
		return sDefaultVal;
}

};