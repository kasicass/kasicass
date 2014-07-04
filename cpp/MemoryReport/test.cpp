#include "mm_report.hpp"
#include <iostream>
#include <memory>
#include <vector>
#include <atomic>

class Foo {
	int a;
};

int main()
{
	Foo *p = mm::New<Foo>();
	std::cout << mm::MemReport() << std::endl;
	mm::Delete(p);
	std::cout << mm::MemReport() << std::endl;

	char *v = (char *) mm::Malloc(10);
	std::cout << mm::MemReport() << std::endl;
	mm::Free(v);
	std::cout << mm::MemReport() << std::endl;

	std::vector<int, mm::allocator<int>> vv;
	vv.push_back(10);
	std::cout << mm::MemReport() << std::endl;
	vv.push_back(20);
	std::cout << mm::MemReport() << std::endl;
	vv.reserve(100);
	std::cout << mm::MemReport() << std::endl;

	std::cout << "V " << sizeof(std::atomic_size_t) << std::endl;
}

