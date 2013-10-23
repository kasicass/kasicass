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

int main(int argc, char* argv[])
{
	Ux::WindowPtr wnd = Ux::createWindow<MyWindow>(IDR_BGIMAGE);
	wnd->show();
	return 0;
}
