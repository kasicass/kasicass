/*
 * tga file format
 * http://www.paulbourke.net/dataformats/tga/
 */

#include <stdio.h>

#pragma pack( push, 1 )
typedef struct {
   char  idlength;
   char  colourmaptype;
   char  datatypecode;
   short int colourmaporigin;
   short int colourmaplength;
   char  colourmapdepth;
   short int x_origin;
   short int y_origin;
   short width;
   short height;
   char  bitsperpixel;
   char  imagedescriptor;
} TGA_HEADER;
#pragma pack( pop )

#pragma pack( push, 1 )
typedef struct {
	char b, g, r, a;
} TGA_COLOR;
#pragma pack( pop )

/* argv[1] - input tga */
/* argv[2] - output tga */
int main(int argc, char* argv[])
{
	FILE *inputFile;
	FILE *outputFile;
	TGA_HEADER header;
	TGA_COLOR color;
	
	if (argc != 3)
	{
		printf("usage: %s <input tga> <output tga>\n", argv[0]);
		return 0;
	}

	inputFile = fopen(argv[1], "rb");
	fread(&header, sizeof(header), 1, inputFile);

	printf("datatypecode: %d\n", header.datatypecode);
	printf("width: %d, height: %d\n", header.width, header.height);
	printf("bitsperpixel: %d\n", header.bitsperpixel);

	if (header.datatypecode != 2)
	{
		printf("support 'Uncompressed, RGB images' only\n");
		return 0;
	}

	if (header.bitsperpixel != 32)
	{
		printf("support 'bitsperpixel == 32' only\n");
		return 0;
	}

	outputFile = fopen(argv[2], "wb+");
	fwrite(&header, sizeof(header), 1, outputFile);

	while (!feof(inputFile))
	{
		fread(&color, sizeof(color), 1, inputFile);
		color.a = 255;
		fwrite(&color, sizeof(color), 1, outputFile);
	}

	fclose(inputFile);
	fclose(outputFile);

	printf("finish~\n");
	return 0;
}