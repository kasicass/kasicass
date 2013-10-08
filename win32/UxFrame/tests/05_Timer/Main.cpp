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
		pbar->x(25);
		pbar->y(100);
		pbar->percent(0.0f);
		this->addComponent(pbar);

		tmr1_ = Ux::createTimer(0.25f, [pbar] () {
			float p = pbar->percent() + 0.005f;
			if (p > 1.0f) p = 0.0f;
			pbar->percent(p);
		});
		tmr1_->start();
	}

private:
	Ux::TimerPtr tmr1_;
};

int main(int argc, char* argv[])
{
	Ux::WindowPtr wnd = Ux::createWindow<MyWindow>(IDR_BGIMAGE);
	wnd->show();
	return 0;
}