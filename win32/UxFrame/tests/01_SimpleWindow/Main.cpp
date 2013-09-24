#include <UxFrame.hpp>

class MyApp : public Ux::App
{
public:
	virtual void onInit()
	{
		wnd_ = Ux::CreateWindow();
		wnd_->show();
	}

private:
	Ux::WindowPtr wnd_;
}

MyApp theApp;

/*
int CALLBACK WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
	return 0;
}
*/

