// sample/whitespace.cpp

#ifndef TIXML_USE_STL
#define TIXML_USE_STL
#endif

#include <iostream>
#include <sstream>
#include "tinyxml.h"

using namespace std;

int main()
{
	TiXmlBase::SetCondenseWhiteSpace( false );

	istringstream parse( "<start>This  is    \ntext</start>" );
	TiXmlElement text( "text" );
	parse >> text; // <-- link error

	cout << "Condense white space OFF."		 << endl
	     << "We got: " << text.FirstChild()->Value() << endl;

	// output 'We got: This  is    text'


	TiXmlBase::SetCondenseWhiteSpace( true );

	istringstream parse1( "<start>This  is    \ntext</start>" );
	TiXmlElement text1( "text" );
	parse1 >> text1; //  <-- link error

	cout << "Condense white space ON."		  << endl
	     << "We got: " << text1.FirstChild()->Value() << endl;

	// output 'We got: This is text'

	return 0;
}