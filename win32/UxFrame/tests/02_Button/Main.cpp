#include <UxFrame.hpp>
#include "Resource.h"

class MyWindow : public Ux::Window
{
public:
	MyWindow(UINT id) : Ux::Window(id)
	{
		this->initComponents();
	}

	virtual ~MyWindow()
	{
	}

	void initComponents()
	{
		Ux::ButtonPtr btn = Ux::createButton(IDR_BUTTON_1);
		btn->x(100);
		btn->y(100);
		this->addComponent(btn);
	}
};

class MyApp : public Ux::App
{
public:
	virtual bool onInit()
	{
		wnd_ = Ux::createWindow<MyWindow>(IDR_BGIMAGE);
		wnd_->show();

		return true;
	}

private:
	Ux::WindowPtr wnd_;
};

MyApp theApp;
