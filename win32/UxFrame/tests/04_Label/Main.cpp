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
		Ux::LabelPtr lbl = Ux::createLabel();
		lbl->text(L"Hello Boy! 中文有问题？");
		lbl->fontColor(Gdiplus::Color(255, 0, 0));
		lbl->fontSize(18.0f);
		//lbl->fontFamily(L"宋体");
		lbl->x(50);
		lbl->y(100);
		this->addComponent(lbl);
	}
};

int main(int argc, char* argv[])
{
	Ux::WindowPtr wnd = Ux::createWindow<MyWindow>(IDR_BGIMAGE);
	wnd->show();
	return 0;
}