#include "student.h"
#include <iostream>

Student::Student() : value_(10)
{
}

void Student::sayHello()
{
	std::cout << "value: " << value_ << std::endl;
}

