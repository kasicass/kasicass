#include <UxFrame.hpp>

class MyApp : public Ux::App
{
public:
	virtual bool onInit()
	{
		wnd_ = Ux::createWindow();
		wnd_->show();
		return true;
	}

private:
	Ux::WindowPtr wnd_;
};

MyApp theApp;
