#pragma once
#include "resource.h"

using namespace System;
using namespace System::Globalization;
using namespace System::ComponentModel::Composition;
using namespace System::Windows::Media;
using namespace Illusion;
using namespace YoZuo::Workbench;

[MenuPart(BaseMenu=STRING(PART_VIDEOSYSTEM))]
public ref class VideoTypePart : public MenuPart
{
public:
	VideoTypePart(void);
};

