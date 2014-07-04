#include "mm_report.hpp"
#include <iostream>
#include <memory>
#include <vector>

class Foo {
	int a;
};

int main()
{
	Foo *p = mm::New<Foo>(mm::TAG_FOO);
	std::cout << mm::MemReport() << std::endl;
	mm::Delete(p);
	std::cout << mm::MemReport() << std::endl;

	char *v = (char *) mm::Malloc(mm::TAG_BUFFER, 10);
	std::cout << mm::MemReport() << std::endl;
	mm::Free(v);
	std::cout << mm::MemReport() << std::endl;

	std::vector<int, mm::allocator<int, mm::TAG_VECTOR_INT>> vv;
	vv.push_back(10);
	std::cout << mm::MemReport() << std::endl;
	vv.push_back(20);
	std::cout << mm::MemReport() << std::endl;
	vv.reserve(100);
	std::cout << mm::MemReport() << std::endl;
}

