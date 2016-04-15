#include "mymd2.h"
#include <stdio.h>
#include <stdlib.h>
#include <err.h>

#include <GL/gl.h>
#include <GL/glu.h>

//=================================================================
//												DEFINE(s)
//=================================================================

#define	MAGIC_IDENT		(('2'<<24) | ('P'<<16) | ('D'<<8) | 'I')
#define	SKIN_NAME_LEN	64

char * IDENT2STR(int ident) {
	static char s[5];
	s[4] = '\0';
	s[3] = (ident >> 24) & 0xFF;
	s[2] = (ident >> 16) & 0xFF;
	s[1] = (ident >>  8) & 0xFF;
	s[0] = ident & 0xFF;
	return s;
}

typedef struct {
	int ident;		// magic number. must be equal to 'IDP2'
	int version;	// md2 version. must be equal to 8

	int skinwidth;	// width of the texture
	int skinheight;	// height of the texture
	int framesize;	// size of one frame in bytes

	int num_skins;	// number of textures
	int num_xyz;	// number of vertices
	int num_st;		// number of texture coordinates
	int num_tris;	// number of triangles
	int num_glcmds;	// number of opengl commands
	int num_frames;	// total number of frames

	int ofs_skins;	// offset to skin names (64 bytes each)
	int ofs_st;		// offset to s-t texture coordinates
	int ofs_tris;	// offset to triangles
	int ofs_frames;	// offset to frame data
	int ofs_glcmds;	// offset to opengl commands
	int ofs_end;	// offset to end of file
} md2_header_t;

typedef float vec3_t[3];

// vertex
typedef struct {
	unsigned char v[3];				// compressed vertex (x, y, z) coordinates
	unsigned char lightnormalindex;	// index to a normal vertex for lighting
} vertex_t;

typedef struct {
	short s;
	short t;
} tex_coord_t;

typedef struct {
	float    scale[3];		// scale values
	float    translate[3];	// translation vector
	char     name[16];		// frame name
	vertex_t verts[1];		// first vertex of this frame
} frame_t;

typedef struct {
	short index_xyz[3];
	short index_st[3];
} triangle_t;


typedef struct {
	md2_header_t hdr;
	frame_t *frames;
	triangle_t *tris;
} _md2_t;

//=================================================================
//												FUNC(s)
//=================================================================

md2_t md2_open(const char *filename)
{
	_md2_t *h = malloc(sizeof(_md2_t));
	md2_header_t *hdr = &h->hdr;
	FILE *fp;

	fp = fopen(filename, "rb");
	if (fp == NULL)
		err(1, "fopen() fail");

	fread(hdr, sizeof(md2_header_t), 1, fp);
	if (hdr->ident != MAGIC_IDENT)
		err(1, "MAGIC NUMBER err");

	// read frames
	h->frames = (frame_t *) malloc(hdr->framesize * hdr->num_frames);

	fseek(fp, hdr->ofs_frames, SEEK_SET);
	fread(h->frames, hdr->framesize, hdr->num_frames, fp);

	// read triangle index(s)
	h->tris = (triangle_t *) malloc(sizeof(triangle_t) * hdr->num_tris);

	fseek(fp, hdr->ofs_tris, SEEK_SET);
	fread(h->tris, sizeof(triangle_t), hdr->num_tris, fp);
	
	fclose(fp);
	return h;
}

void md2_close(md2_t _h) {
	_md2_t *h = (_md2_t *)_h;

//	free(h->tris);
//	free(h->frames);
	free(h);
}

void md2_desc(md2_t _h)
{
	md2_header_t *h = &((_md2_t *)_h)->hdr;
	printf("ident:      %s\n", IDENT2STR(h->ident));
	printf("version:    %d\n", h->version);

	printf("skinwidth:  %d\n", h->skinwidth);
	printf("skinheight: %d\n", h->skinheight);
	printf("framesize:  %d\n", h->framesize);

	printf("num_skins:  %d\n", h->num_skins);
	printf("num_xyz:    %d\n", h->num_xyz);
	printf("num_st:     %d\n", h->num_st);
	printf("num_tris:   %d\n", h->num_tris);
	printf("num_glcmds: %d\n", h->num_glcmds);
	printf("num_frames: %d\n", h->num_frames);

	printf("ofs_skins:  %d\n", h->ofs_skins);
	printf("ofs_st:     %d\n", h->ofs_st);
	printf("ofs_tris:   %d\n", h->ofs_tris);
	printf("ofs_frames: %d\n", h->ofs_frames);
	printf("ofs_glcmds: %d\n", h->ofs_glcmds);
	printf("ofs_end:    %d\n", h->ofs_end);
}

void md2_frame_sample(md2_t _h)
{
	_md2_t *h = (_md2_t *)_h; 
	md2_header_t *hdr = &h->hdr;
	frame_t *frames = h->frames;
	triangle_t *tris = h->tris;
	int k = 0;

		// draw each triangle
		for ( int i = 0; i < 10; i++ )
		{
			// draw triangle #i
			for ( int j = 0; j < 3; j++ )
			{
				// k is the frame to draw
				// i is the current triangle of the frame
				// j is the current vertex of the triangle

				printf("%f,%f,%f\n", (frames[k].verts[ tris[i].index_xyz[j] ].v[0] * frames[k].scale[0]) + frames[k].translate[0],
						            (frames[k].verts[ tris[i].index_xyz[j] ].v[1] * frames[k].scale[1]) + frames[k].translate[1],
						            (frames[k].verts[ tris[i].index_xyz[j] ].v[2] * frames[k].scale[2]) + frames[k].translate[2] );
			}
		}
}

void md2_draw(md2_t _h)
{
	_md2_t *h = (_md2_t *)_h; 
	md2_header_t *hdr = &h->hdr;
	frame_t *frames = h->frames;
	triangle_t *tris = h->tris;
	int k = 0;

	glBegin(GL_TRIANGLES);
		// draw each triangle
		for ( int i = 0; i < hdr->num_tris; i++ )
		{
			// draw triangle #i
			for ( int j = 0; j < 3; j++ )
			{
				// k is the frame to draw
				// i is the current triangle of the frame
				// j is the current vertex of the triangle

				glVertex3f( (frames[k].verts[ tris[i].index_xyz[j] ].v[0] * frames[k].scale[0]) + frames[k].translate[0],
				            (frames[k].verts[ tris[i].index_xyz[j] ].v[1] * frames[k].scale[1]) + frames[k].translate[1],
				            (frames[k].verts[ tris[i].index_xyz[j] ].v[2] * frames[k].scale[2]) + frames[k].translate[2] );
			}
		}
	glEnd();
}

