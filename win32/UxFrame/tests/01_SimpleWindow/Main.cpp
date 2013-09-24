#include <UxFrame.hpp>
#include "Resource.h"

class MyApp : public Ux::App
{
public:
	virtual bool onInit()
	{
		wnd_ = std::make_shared<Ux::Window>(IDR_BGIMAGE);
		wnd_->show();

		return true;
	}

private:
	Ux::WindowPtr wnd_;
};

MyApp theApp;
