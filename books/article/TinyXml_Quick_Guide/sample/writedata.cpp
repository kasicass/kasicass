// sample/writedata.cpp
#include <iostream>
#include "tinyxml.h"


int main()
{
	TiXmlDocument doc( "demo.xml" );
	bool loadOkay = doc.LoadFile( "demo.xml" );

	if ( !loadOkay )
	{
		printf( "Could not load 'demo.xml'. Error='%s'.\n",
			doc.ErrorDesc() );
		exit( 1 );
	}



	TiXmlNode *	node		= 0;
	TiXmlElement *	dataElement	= 0;
	TiXmlElement *	itemElement	= 0;
	TiXmlText *	itemText	= 0;

	node = doc.FirstChild( "Data" );
	dataElement = node->ToElement();

	//
	// Modify attribute
	//

	// Get first <Player>
	itemElement = dataElement->FirstChildElement();
	itemElement->SetAttribute( "level", 90 );
	itemElement->SetDoubleAttribute( "rate", 0.8 );
	
	itemElement->RemoveAttribute( "desc" );	// remove one
	itemElement->SetAttribute( "new", "new" );

	//
	// Modify TiXmlText value
	//

	// Get second <Player>
	itemElement = itemElement->NextSiblingElement();
	// Get <Name>
	itemElement = itemElement->FirstChildElement();
	// Get TiXmlText 'Gosla'
	itemText = itemElement->FirstChild()->ToText();
	itemText->SetValue( "QQ" );	// 'Gosla' -> 'QQ'


	//
	// Insert new elements
	//

	// Create nodes
	TiXmlElement	newElement( "Player" );
	newElement.SetAttribute( "level", 10 );

	TiXmlText	newText( "Firefox" );

	newElement.InsertEndChild( newText );

	// Insert to the document
	// Get the first <Player>
	itemElement = dataElement->FirstChildElement();

	dataElement->InsertAfterChild( itemElement, newElement );


	// Save file
	doc.SaveFile();

	return 0;
}
