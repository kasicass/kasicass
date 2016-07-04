#pragma once

using namespace System;
using namespace System::Globalization;
using namespace System::ComponentModel::Composition;
using namespace System::Windows::Media;
using namespace Illusion;
using namespace YoZuo::Workbench;

[MenuPart(BaseMenu=WorkbenchName::MonitorSystemTab)]
public ref class VideoSystemGroup : public MenuPart
{
public:
	VideoSystemGroup(void);
};

