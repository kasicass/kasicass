#include <UxFrame.hpp>

class MyApp : public Ux::App
{
public:
	virtual bool onInit()
	{
		//wnd_ = Ux::CreateWindow();
		//wnd_->show();
		::MessageBox(NULL, "Hello", "Good!", MB_OK);
		return true;
	}

	virtual void onShutdown()
	{
	}

private:
	//Ux::WindowPtr wnd_;
};

MyApp theApp;

/*
int CALLBACK WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
	return 0;
}
*/

