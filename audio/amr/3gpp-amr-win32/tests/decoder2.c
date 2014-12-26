/*
********************************************************************************
*                         INCLUDE FILES
********************************************************************************
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifdef _DEBUG
#include <crtdbg.h>
#endif
#include "typedef.h"
#include "n_proc.h"
#include "cnst.h"
#include "mode.h"
#include "frame.h"
#include "strfunc.h"
#include "sp_dec.h"
#include "d_homing.h"

#ifdef MMS_IO
#define AMR_MAGIC_NUMBER "#!AMR\n"
#define AMR_MAGIC_NUMBER_LEN (sizeof(AMR_MAGIC_NUMBER)-1)
#define MAX_PACKED_SIZE (MAX_SERIAL_SIZE / 8 + 2)
#endif

const char decoder_id[] = "@(#)$Id $";

/* frame size in serial bitstream file (frame type + serial stream + flags) */
#define SERIAL_FRAMESIZE (1+MAX_SERIAL_SIZE+5)


/*
********************************************************************************
*                         LOCAL PROGRAM CODE
********************************************************************************
*/

struct OutputBuffer {
	char *data;
	unsigned int len;
};

struct OutputBuffer* OB_New()
{
	struct OutputBuffer* ob = (struct OutputBuffer*) malloc(sizeof(struct OutputBuffer));
	memset(ob, 0, sizeof(struct OutputBuffer));
	return ob;
}

void OB_Delete(struct OutputBuffer* ob)
{
	if (ob->data) { free(ob->data); ob->data = NULL; }
	free(ob);
}

void OB_Append(struct OutputBuffer* ob, const char* data, unsigned int len)
{
	ob->data = (char*)realloc(ob->data, ob->len + len);
	memcpy(ob->data + ob->len, data, len);
	ob->len += len;
}

struct OutputBuffer* decodeAMRtoPCM(const char *buf, unsigned int len)
{
	Speech_Decode_FrameState *speech_decoder_state = NULL;

	const char *curr_buf  = buf;
	unsigned int curr_len = len;
	struct OutputBuffer *ob = NULL;

	Word16 serial[SERIAL_FRAMESIZE];   /* coded bits                    */
	Word16 synth[L_FRAME];             /* Synthesis                     */
	const unsigned int frame_size = sizeof(Word16)*L_FRAME;

	int rxframetypeMode = 0;           /* use RX frame type codes       */
	enum Mode mode = (enum Mode)0;
	enum RXFrameType rx_type = (enum RXFrameType)0;
	enum TXFrameType tx_type = (enum TXFrameType)0;
     
	Word16 reset_flag = 0;
	Word16 reset_flag_old = 1;
	Word16 i;
  
#ifdef MMS_IO
	UWord8 toc, q, ft;
	UWord8 packed_bits[MAX_PACKED_SIZE];
	Word16 packed_size[16] = {12, 13, 15, 17, 19, 20, 26, 31, 5, 0, 0, 0, 0, 0, 0, 0};
#endif

#ifdef MMS_IO
	/* read and verify magic number */
	if (strncmp(curr_buf, AMR_MAGIC_NUMBER, AMR_MAGIC_NUMBER_LEN) != 0)
	{
		return NULL;
	}

	curr_buf += AMR_MAGIC_NUMBER_LEN;
	curr_len -= AMR_MAGIC_NUMBER_LEN;
#endif

	if (Speech_Decode_Frame_init(&speech_decoder_state, "Decoder"))
	{
		return NULL;
	}

	ob = OB_New();
#ifndef MMS_IO
  while (fread (serial, sizeof (Word16), SERIAL_FRAMESIZE, file_serial)
         == SERIAL_FRAMESIZE)
  {
     /* get frame type and mode information from frame */
     if (rxframetypeMode) {
         rx_type = (enum RXFrameType)serial[0];
     } else {
         tx_type = (enum TXFrameType)serial[0];
         rx_type = tx_to_rx (tx_type);
     }
     mode = (enum Mode) serial[1+MAX_SERIAL_SIZE];

#else

	while (curr_len > 1)
	{
		toc = *((UWord8*)curr_buf);
		curr_buf += 1;
		curr_len -= 1;

		/* read rest of the frame based on ToC byte */
		q  = (toc >> 2) & 0x01;
		ft = (toc >> 3) & 0x0F;

		memcpy(packed_bits, curr_buf, packed_size[ft]);
		curr_buf += packed_size[ft];
		curr_len -= packed_size[ft];

		rx_type = UnpackBits(q, ft, packed_bits, &mode, &serial[1]);

#endif
		if (rx_type == RX_NO_DATA) {
			mode = speech_decoder_state->prev_mode;
		} else {
			speech_decoder_state->prev_mode = mode;
		}

		/* if homed: check if this frame is another homing frame */
		if (reset_flag_old == 1)
		{
			/* only check until end of first subframe */
			reset_flag = decoder_homing_frame_test_first(&serial[1], mode);
		}

		/* produce encoder homing frame if homed & input=decoder homing frame */
		if ((reset_flag != 0) && (reset_flag_old != 0))
		{
			for (i = 0; i < L_FRAME; i++)
			{
				synth[i] = EHF_MASK;
			}
		}
		else
		{     
			/* decode frame */
			Speech_Decode_Frame(speech_decoder_state, mode, &serial[1], rx_type, synth);
		}

		/* write synthesized speech to file */
		OB_Append(ob, (const char*)synth, frame_size);
		
		/* if not homed: check whether current frame is a homing frame */
		if (reset_flag_old == 0)
		{
			/* check whole frame */
			reset_flag = decoder_homing_frame_test(&serial[1], mode);
		}
		
		/* reset decoder if current frame is a homing frame */
		if (reset_flag != 0)
		{
			Speech_Decode_Frame_reset(speech_decoder_state);
		}
		reset_flag_old = reset_flag;
	}

	Speech_Decode_Frame_exit(&speech_decoder_state);
	return ob;
}

/*
********************************************************************************
*                             MAIN PROGRAM 
********************************************************************************
*/

int main (int argc, char *argv[])
{
	char *buf = NULL;
	unsigned int len = 0;
	FILE *fp = NULL;
	struct OutputBuffer *ob = NULL;

#ifdef _DEBUG
	int flags = _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG);
	flags = (flags & 0x0000FFFF) | _CRTDBG_LEAK_CHECK_DF;
	_CrtSetDbgFlag(flags);
#endif

	if (argc != 3)
	{
		printf("usage:\n");
		printf("  %s <amrfile> <outputfile>\n", argv[0]);
		return 1;
	}

	fp = fopen(argv[1], "rb");
	fseek(fp, 0, SEEK_END);
	len = ftell(fp);
	buf = (char*)malloc(len);
	fseek(fp, 0, SEEK_SET);
	fread(buf, len, 1, fp);
	fclose(fp);

	ob = decodeAMRtoPCM(buf, len);
	free(buf);

	if (!ob)
	{
		printf("decode failed\n");
		return 1;
	}

	fp = fopen(argv[2], "wb");
	fwrite(ob->data, ob->len, 1, fp);
	fclose(fp);

	OB_Delete(ob);
	printf("done\n");
	return 0;
}
