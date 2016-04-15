// sample/readdata.cpp

#include <stdlib.h>
#include <stdio.h>

#include <string>
#include "tinyxml.h"


int main()
{
	TiXmlDocument	doc; // ( "demo.xml" );
	bool loadOkay = doc.LoadFile( "demo.xml" );

	if ( !loadOkay )
	{
		printf( "Could not load 'demo.xml'. Error='%s'.\n",
			doc.ErrorDesc() );
		exit( 1 );
	}


	//--------------------------------------
	// begin to read data
	//--------------------------------------
	
	TiXmlNode *	node		= 0;
	TiXmlElement *	dataElement	= 0;
	TiXmlElement *	itemElement	= 0;
	TiXmlComment *	itemComment	= 0;
	TiXmlText *	itemText	= 0;

	int		level = 0;
	double		rate  = 0;
	std::string	name, desc;

	// Get the "Data" element.
	// It is a child of the document, and can be selected by name.
	node = doc.FirstChild( "Data" );
	dataElement = node->ToElement();

	
	// Get the comment
	node = dataElement->FirstChild();
	itemComment = node->ToComment();

	printf( "comment: %s\n", itemComment->Value() );


	// Get the first "Player"
	// Using "FirstChildElement" can skip the comment.
	node = dataElement->FirstChildElement();
	itemElement = node->ToElement();
	itemText = itemElement->FirstChild()->ToText();

	// query attributes
	itemElement->QueryIntAttribute( "level", &level );
	itemElement->QueryDoubleAttribute( "rate", &rate );
	desc = itemElement->Attribute( "desc" );
	name = itemText->Value();

	// Output infomation
	printf( "name: %s, level: %d, rate: %f, desc: %s\n",
		name.c_str(), level, rate, desc.c_str() );
	

	// Get the second "Player"
	// IMPORTANT: It's itemElement::NextSiblingElement() been called, not dataElement's.
	itemElement = itemElement->NextSiblingElement();

	// Get name
	itemElement = itemElement->FirstChildElement();
	itemText = itemElement->FirstChild()->ToText();
	name = itemText->Value();

	// Get level
	itemElement = itemElement->NextSiblingElement();
	itemText = itemElement->FirstChild()->ToText();
	level = atoi( itemText->Value() );

	// Get rate
	itemElement = itemElement->NextSiblingElement();
	itemText = itemElement->FirstChild()->ToText();
	rate = atof( itemText->Value() );

	// Get desc
	itemElement = itemElement->NextSiblingElement();
	itemText = itemElement->FirstChild()->ToText();
	desc = itemText->Value();


	// Output infomation
	printf( "name: %s, level: %d, rate: %f, desc: %s\n",
		name.c_str(), level, rate, desc.c_str() );


	return 0;
}
