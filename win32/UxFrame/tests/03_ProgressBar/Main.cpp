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
		Ux::ProgressBarPtr pbar = Ux::createProgressBar(IDR_PROGRESS_FG, IDR_PROGRESS_BG);
		pbar->x(50);
		pbar->y(200);
		pbar->percent(0.3f);
		this->addComponent(pbar);
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
