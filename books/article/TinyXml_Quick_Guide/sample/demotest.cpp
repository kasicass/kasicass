// sample/demotest.cpp
#include <stdlib.h>
#include <stdio.h>
#include "tinyxml.h"

// XML file content
const char * demoStart = 
	"<?xml version=\"1.0\" ?>\n"
	"<!-- This is comment here. -->\n"
	"<Data>\n"
	"<Player level=\"12\">Kasi</Player>\n"
	"<Player>Gosla</Player>\n"
	"</Data>\n";

int main()
{
	// Write to a file and read it back, to check file I/O.

	TiXmlDocument OutputDoc( "demotest.xml" );
	OutputDoc.Parse( demoStart );

	if ( OutputDoc.Error() )
	{
		printf( "Error in %s: %s\n", 
			OutputDoc.Value(), OutputDoc.ErrorDesc() );
		exit( 1 );
	}
	OutputDoc.SaveFile();


	TiXmlDocument InputDoc( "demotest.xml" );
	bool loadOkay = InputDoc.LoadFile();

	if ( !loadOkay )
	{
		printf( "Could not load 'demotest.xml': Error='%s'.\n",
			InputDoc.ErrorDesc() );
		exit( 1 );
	}

	return 0;
}
