/*
*****************************************************************************
*                         INCLUDE FILES
*****************************************************************************
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifdef _DEBUG
#include <crtdbg.h>
#endif
#include "typedef.h"
#include "cnst.h"
#include "n_proc.h"
#include "mode.h"
#include "frame.h"
#include "strfunc.h"
#include "sp_enc.h"
#include "pre_proc.h"
#include "sid_sync.h"
#include "vadname.h"
#include "e_homing.h"

#ifdef MMS_IO
#define AMR_MAGIC_NUMBER "#!AMR\n"
#define AMR_MAGIC_NUMBER_LEN (sizeof(AMR_MAGIC_NUMBER)-1)
#define MAX_PACKED_SIZE (MAX_SERIAL_SIZE / 8 + 2)
#endif

const char coder_id[] = "@(#)$Id $";

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

struct OutputBuffer* encodePCMtoAMR(const char *buf, unsigned int len)
{
	const char* curr_buf = buf;
	unsigned int curr_len = len;
	struct OutputBuffer* ob = NULL;

	Speech_Encode_FrameState *speech_encoder_state = NULL;
	sid_syncState *sid_state = NULL;

	//Word16 new_speech[L_FRAME];         /* Pointer to new speech data        */
	Word16 *new_speech = NULL;
	Word16 serial[SERIAL_FRAMESIZE];    /* Output bitstream buffer           */

#ifdef MMS_IO
	UWord8 packed_bits[MAX_PACKED_SIZE];
	Word16 packed_size;
#endif

	int i;
	Word16 dtx = 0;
	const unsigned int frame_size = sizeof(Word16)*L_FRAME;

	enum Mode mode = MR475;
	enum Mode used_mode;
	enum TXFrameType tx_type;
	
	/* changed eedodr */
	Word16 reset_flag;
	
  /*-----------------------------------------------------------------------*
   * Initialisation of the coder.                                          *
   *-----------------------------------------------------------------------*/
	if (Speech_Encode_Frame_init(&speech_encoder_state, dtx, "encoder") ||
		sid_sync_init(&sid_state))
	{
		return NULL;
	}

	ob = OB_New();

#ifdef MMS_IO
	/* write magic number to indicate single channel AMR file storage format */
	OB_Append(ob, AMR_MAGIC_NUMBER, AMR_MAGIC_NUMBER_LEN);
#endif

	/*-----------------------------------------------------------------------*
	* Process speech frame by frame                                         *
	*-----------------------------------------------------------------------*/
	while (curr_len >= frame_size)
	{
		// memcpy(new_speech, curr_buf, frame_size);
		new_speech = (Word16*)curr_buf;
		curr_buf += frame_size;
		curr_len -= frame_size;

		/* zero flags and parameter bits */
		for (i = 0; i < SERIAL_FRAMESIZE; i++)
			serial[i] = 0;

		/* check for homing frame */
		reset_flag = encoder_homing_frame_test(new_speech);

		/* encode speech */
		Speech_Encode_Frame(speech_encoder_state, mode,
			new_speech, &serial[1], &used_mode);

		/* include frame type and mode information in serial bitstream */
		sid_sync(sid_state, used_mode, &tx_type);

#ifndef MMS_IO
     serial[0] = tx_type;
     if (tx_type != TX_NO_DATA) {
       serial[1+MAX_SERIAL_SIZE] = mode;
     }
     else {
       serial[1+MAX_SERIAL_SIZE] = -1;
     }

     /* write bitstream to output file */
     if (fwrite (serial, sizeof (Word16), SERIAL_FRAMESIZE, file_serial)
         != SERIAL_FRAMESIZE) {
         fprintf(stderr, "\nerror writing output file: %s\n",
                 strerror(errno));
         exit(-1);
     }
#else
		packed_size = PackBits(used_mode, mode, tx_type, &serial[1], packed_bits);

		/* write file storage format bitstream to output file */
		OB_Append(ob, (const char *)packed_bits, packed_size);
#endif

		/* perform homing if homing frame was detected at encoder input */
		if (reset_flag != 0)
		{
			Speech_Encode_Frame_reset(speech_encoder_state);
			sid_sync_reset(sid_state);
		}
	}

	Speech_Encode_Frame_exit(&speech_encoder_state);
	sid_sync_exit(&sid_state);

	return ob;
}

/*
*****************************************************************************
*                             MAIN PROGRAM 
*****************************************************************************
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
		printf("  %s <pcmfile> <outputfile>\n", argv[0]);
		return 1;
	}

	fp = fopen(argv[1], "rb");
	fseek(fp, 0, SEEK_END);
	len = ftell(fp);
	buf = (char*)malloc(len);
	fseek(fp, 0, SEEK_SET);
	fread(buf, len, 1, fp);
	fclose(fp);

	ob = encodePCMtoAMR(buf, len);
	free(buf);

	if (!ob)
	{
		printf("encode failed\n");
		return 1;
	}

	fp = fopen(argv[2], "wb");
	fwrite(ob->data, ob->len, 1, fp);
	fclose(fp);

	OB_Delete(ob);
	printf("done\n");
	return 0;
}
