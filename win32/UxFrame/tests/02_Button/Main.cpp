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
		Ux::ButtonPtr btn = Ux::createButton(IDR_BUTTON_NORMAL,
			IDR_BUTTON_DOWN, IDR_BUTTON_OVER, IDR_BUTTON_DISABLE);
		btn->x(200);
		btn->y(100);
		btn->setClickFunc([](Ux::Button *self) {
			self->disable(true);
		});
		this->addComponent(btn);
	}
};

int main(int argc, char* argv[])
{
	Ux::WindowPtr wnd = Ux::createWindow<MyWindow>(IDR_BGIMAGE);
	wnd->show();
	return 0;
}
