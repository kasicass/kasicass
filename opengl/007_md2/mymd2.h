#ifndef MY_MD2_H
#define MY_MD2_H

typedef void* md2_t;

md2_t md2_open(const char *filename);
void md2_close(md2_t h);
void md2_desc(md2_t h);

void md2_frame_sample(md2_t _h);
void md2_draw(md2_t h);

#endif

