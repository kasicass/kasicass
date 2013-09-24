#pragma once

namespace Ux {

class App
{
public:
	App();
	virtual ~App();

	virtual bool onInit() = 0;
	virtual void onShutdown() = 0;

public:
	static App& instance();

private:
	static App* s_app;
};

}