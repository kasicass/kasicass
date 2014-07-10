#pragma once

#include <string>
#include <vector>
#include <map>
#include <unordered_map>

namespace mm {

#if defined(MEMORY_REPORT_ENABLE)

template <RECORD_TAG tag>
class string : public std::basic_string<char, std::char_traits<char>, allocator<char, tag>>
{};

template <RECORD_TAG tag>
class wstring : public std::basic_string<wchar_t, std::char_traits<wchar_t>, allocator<wchar_t, tag>>
{};

template <typename V, RECORD_TAG tag>
class vector : public std::vector<V, allocator<V, tag>>
{};

template <typename K, typename V, RECORD_TAG tag>
class map : public std::map<K, V, std::less<K>, allocator<std::pair<const K, V>, tag>>
{};

template <typename K, typename V, RECORD_TAG tag>
class unordered_map : public std::unordered_map<K, V, std::hash<K>, std::equal_to<K>, allocator<std::pair<const K, V>, tag>>
{};

#else

template <typename V, RECORD_TAG tag>
class vector : public std::vector<V>
{};

template <typename K, typename V, RECORD_TAG tag>
class map : public std::map<K, V>
{};

template <typename K, typename V, RECORD_TAG tag>
class unordered_map : public std::unordered_map<K, V>
{};

#endif

}