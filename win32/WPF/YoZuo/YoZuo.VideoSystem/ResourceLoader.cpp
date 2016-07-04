#include "StdAfx.h"
#include "resource.h"
#include "ResourceLoader.h"
#include <msclr\marshal_cppstd.h>

using namespace msclr::interop;
using namespace System::Reflection;
using namespace System::Runtime::InteropServices;

const int MAX_LOADSTRING = 100;

ResourceLoader::ResourceLoader(void)
{
}

static HINSTANCE instance = (HINSTANCE)Marshal::GetHINSTANCE(Assembly::GetExecutingAssembly()->GetModules()[0]).ToPointer();

String^ ResourceLoader::GetString( String^ name )
{
	UInt32 i;
	if(!UInt32::TryParse(name, i))
	{
		return nullptr;
	}
	wchar_t str[MAX_LOADSTRING];
	::LoadString(instance, i, str, MAX_LOADSTRING);	
	return marshal_as<String^>(str);
}

ImageSource^ ResourceLoader::GetImage( String^ name )
{
	//1. LoadImage from rc
	UInt32 i;
	if(!UInt32::TryParse(name, i))
	{
		return nullptr;
	}
	marshal_context ^ context = gcnew marshal_context();
	PCWSTR pszString = context->marshal_as<const wchar_t*>(name);
	HBITMAP hBitmap = ::LoadBitmap(instance, MAKEINTRESOURCE(i));
	delete context;
	if (hBitmap == NULL)
	{
		return nullptr;
	}

	System::Drawing::Bitmap^ bitmap = System::Drawing::Bitmap::FromHbitmap(IntPtr(hBitmap));
	DeleteObject(hBitmap);

	//2. Convert to ImageSource
	Imaging::BitmapSource^ bs = System::Windows::Interop::Imaging::CreateBitmapSourceFromHBitmap(
		bitmap->GetHbitmap(),
		IntPtr::Zero,
		System::Windows::Int32Rect::Empty,
		Imaging::BitmapSizeOptions::FromEmptyOptions());
	DeleteObject((HGDIOBJ)bitmap->GetHbitmap());

	return bs;
}

void ResourceLoader::CurrentCulture::set(CultureInfo^ culture)
{
	if (culture == nullptr)
	{
		return;
	}

	if (m_culture != culture)
	{
		m_culture = culture;
		::SetThreadLocale(culture->LCID);
		::SetThreadUILanguage(culture->LCID);
	}
};