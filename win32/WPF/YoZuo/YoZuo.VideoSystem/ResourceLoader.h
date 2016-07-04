#pragma once

using namespace System;
using namespace System::Globalization;
using namespace System::ComponentModel::Composition;
using namespace System::Windows::Media;
using namespace Illusion;

[Export(IResource::typeid)]
public ref class ResourceLoader : public IResource
{
public:
	ResourceLoader(void);

	virtual String^ GetString( String^ name );

	virtual ImageSource^ GetImage( String^ name );

	virtual property CultureInfo^ CurrentCulture { 
		void set (CultureInfo^);
	}

private:
	CultureInfo^ m_culture;
};

