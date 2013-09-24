#pragma once

#include <Windows.h>

namespace Ux {

class App
{
public:
	App();
	virtual ~App();

	virtual bool onInit() = 0;

	void run();

	HINSTANCE getHINSTANCE();
	void setHINSTANCE(HINSTANCE hInstance);

private:
	HINSTANCE hInstance_;

public:
	static App& instance();

private:
	static App* s_app;
};

}