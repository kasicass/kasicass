/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : dtx_enc.c
*      Purpose          : DTX mode computation of SID parameters
*
********************************************************************************
*/

/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "dtx_enc.h"
const char dtx_enc_id[] = "@(#)$Id $" dtx_enc_h;
 
/*
********************************************************************************
*                         INCLUDE FILES
********************************************************************************
*/
#include <stdlib.h>
#include <stdio.h>
#include "q_plsf.h"
#include "typedef.h"
#include "basic_op.h"
#include "oper_32b.h"
#include "copy.h"
#include "set_zero.h"
#include "mode.h"
#include "log2.h"
#include "lsp_lsf.h"
#include "reorder.h"
#include "count.h"

/*
********************************************************************************
*                         LOCAL VARIABLES AND TABLES
********************************************************************************
*/
#include "lsp.tab"

/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
/*
**************************************************************************
*
*  Function    : dtx_enc_init
*
**************************************************************************
*/ 
int dtx_enc_init (dtx_encState **st)
{
  dtx_encState* s;
 
  if (st == (dtx_encState **) NULL){
    fprintf(stderr, "dtx_enc_init: invalid parameter\n");
    return -1;
  }
  
  *st = NULL;
 
  /* allocate memory */
  if ((s= (dtx_encState *) malloc(sizeof(dtx_encState))) == NULL){
    fprintf(stderr, "dtx_enc_init: can not malloc state structure\n");
    return -1;
  }
  
  dtx_enc_reset(s);
  *st = s;
  
  return 0;
}
 
/*
**************************************************************************
*
*  Function    : dtx_enc_reset
*
**************************************************************************
*/
int dtx_enc_reset (dtx_encState *st)
{
  Word16 i;

  if (st == (dtx_encState *) NULL){
    fprintf(stderr, "dtx_enc_reset: invalid parameter\n");
    return -1;
  }

  st->hist_ptr = 0;
  st->log_en_index = 0;
  st->init_lsf_vq_index = 0;
  st->lsp_index[0] = 0;
  st->lsp_index[1] = 0;
  st->lsp_index[2] = 0;
 
  /* Init lsp_hist[] */
  for(i = 0; i < DTX_HIST_SIZE; i++)
  {
    Copy(lsp_init_data, &st->lsp_hist[i * M], M);
  }

  /* Reset energy history */
  Set_zero(st->log_en_hist, M);

  st->dtxHangoverCount = DTX_HANG_CONST;
  st->decAnaElapsedCount = 32767; 

  return 1;
}
 
/*
**************************************************************************
*
*  Function    : dtx_enc_exit
*
**************************************************************************
*/
void dtx_enc_exit (dtx_encState **st)
{
   if (st == NULL || *st == NULL)
      return;
   
   /* deallocate memory */
   free(*st);
   *st = NULL;
   
   return;
}
 
/*
**************************************************************************
*
*  Function    : dtx_enc
*                
**************************************************************************
*/
int dtx_enc(dtx_encState *st,        /* i/o : State struct                    */
            Word16 computeSidFlag,   /* i   : compute SID                     */
            Q_plsfState *qSt,        /* i/o : Qunatizer state struct          */
            gc_predState* predState, /* i/o : State struct                    */
	    Word16 **anap            /* o   : analysis parameters             */
	    )
{
   Word16 i,j;
   Word16 log_en;
   Word16 lsf[M];
   Word16 lsp[M];
   Word16 lsp_q[M];
   Word32 L_lsp[M];

   /* VOX mode computation of SID parameters */
   test (); test ();
   if ((computeSidFlag != 0))
   {
      /* compute new SID frame if safe i.e don't
       * compute immediately after a talk spurt  */
      log_en = 0;                                           move16 ();
      for (i = 0; i < M; i++)
      {
         L_lsp[i] = 0;                                      move16 ();
      }
      
      /* average energy and lsp */
      for (i = 0; i < DTX_HIST_SIZE; i++)
      {
         log_en = add(log_en,
                      shr(st->log_en_hist[i],2));

         for (j = 0; j < M; j++)
         {
            L_lsp[j] = L_add(L_lsp[j],
                             L_deposit_l(st->lsp_hist[i * M + j]));
         }
      }

      log_en = shr(log_en, 1);
      for (j = 0; j < M; j++)
      {
         lsp[j] = extract_l(L_shr(L_lsp[j], 3));   /* divide by 8 */
      }

      /*  quantize logarithmic energy to 6 bits */
      st->log_en_index = add(log_en, 2560);          /* +2.5 in Q10      */
      st->log_en_index = add(st->log_en_index, 128); /* add 0.5/4 in Q10 */
      st->log_en_index = shr(st->log_en_index, 8);

      test ();
      if (sub(st->log_en_index, 63) > 0)
      {
         st->log_en_index = 63;                             move16 ();
      }
      test ();
      if (st->log_en_index < 0)
      {
         st->log_en_index = 0;                              move16 ();
      }
         
      /* update gain predictor memory */
      log_en = shl(st->log_en_index, -2+10); /* Q11 and divide by 4 */
      log_en = sub(log_en, 2560);            /* add 2.5 in Q11      */
      
      log_en = sub(log_en, 9000);
      test ();
      if (log_en > 0)
      {
         log_en = 0;                                        move16 ();
      }
      test ();
      if (sub(log_en, -14436) < 0)
      {
         log_en = -14436;                                   move16 ();
      }
      
      /* past_qua_en for other modes than MR122 */      
      predState->past_qua_en[0] = log_en;                   move16 ();
      predState->past_qua_en[1] = log_en;                   move16 ();
      predState->past_qua_en[2] = log_en;                   move16 ();
      predState->past_qua_en[3] = log_en;                   move16 ();

      /* scale down by factor 20*log10(2) in Q15 */
      log_en = mult(5443, log_en);
      
      /* past_qua_en for mode MR122 */      
      predState->past_qua_en_MR122[0] = log_en;             move16 ();
      predState->past_qua_en_MR122[1] = log_en;             move16 ();
      predState->past_qua_en_MR122[2] = log_en;             move16 ();
      predState->past_qua_en_MR122[3] = log_en;             move16 ();
 
      /* make sure that LSP's are ordered */
      Lsp_lsf(lsp, lsf, M);
      Reorder_lsf(lsf, LSF_GAP, M);
      Lsf_lsp(lsf, lsp, M);
      
      /* Quantize lsp and put on parameter list */
      Q_plsf_3(qSt, MRDTX, lsp, lsp_q, st->lsp_index, 
               &st->init_lsf_vq_index);
   }
   
   *(*anap)++ = st->init_lsf_vq_index; /* 3 bits */         move16 ();
   
   *(*anap)++ = st->lsp_index[0];      /* 8 bits */         move16 ();
   *(*anap)++ = st->lsp_index[1];      /* 9 bits */         move16 ();
   *(*anap)++ = st->lsp_index[2];      /* 9 bits */         move16 ();
   
   
   *(*anap)++ = st->log_en_index;      /* 6 bits    */      move16 ();
                                       /* = 35 bits */
   
   return 0;
}

/*
**************************************************************************
*
*  Function    : dtx_buffer
*  Purpose     : handles the DTX buffer
*                
**************************************************************************
*/
int dtx_buffer(dtx_encState *st,   /* i/o : State struct                    */
	       Word16 lsp_new[],   /* i   : LSP vector                      */
	       Word16 speech[]     /* i   : speech samples                  */
	       )
{
   Word16 i;
   Word32 L_frame_en;
   Word16 log_en_e;
   Word16 log_en_m;
   Word16 log_en;
   
   /* update pointer to circular buffer      */
   st->hist_ptr = add(st->hist_ptr, 1);
   test ();
   if (sub(st->hist_ptr, DTX_HIST_SIZE) == 0)
   {
      st->hist_ptr = 0;                                     move16 ();
   }
   
   /* copy lsp vector into buffer */
   Copy(lsp_new, &st->lsp_hist[st->hist_ptr * M], M);
   
   /* compute log energy based on frame energy */
   L_frame_en = 0;     /* Q0 */                             move32 ();
   for (i=0; i < L_FRAME; i++)
   {
      L_frame_en = L_mac(L_frame_en, speech[i], speech[i]); 
   }
   Log2(L_frame_en, &log_en_e, &log_en_m);
   
   /* convert exponent and mantissa to Word16 Q10 */
   log_en = shl(log_en_e, 10);  /* Q10 */
   log_en = add(log_en, shr(log_en_m, 15-10));
   
   /* divide with L_FRAME i.e subtract with log2(L_FRAME) = 7.32193 */
   log_en = sub(log_en, 8521);
   
   /* insert into log energy buffer with division by 2 */
   log_en = shr(log_en, 1);
   st->log_en_hist[st->hist_ptr] = log_en; /* Q10 */        move16 ();

   return 0;
}

/*
**************************************************************************
*
*  Function    : tx_dtx_handler
*  Purpose     : adds extra speech hangover to analyze speech on the decoding side. 
*                
**************************************************************************
*/
Word16 tx_dtx_handler(dtx_encState *st,      /* i/o : State struct           */
                      Word16 vad_flag,       /* i   : vad decision           */
                      enum Mode *usedMode    /* i/o : mode changed or not    */
                      )
{
   Word16 compute_new_sid_possible;
   
   /* this state machine is in synch with the GSMEFR txDtx machine      */ 
   st->decAnaElapsedCount = add(st->decAnaElapsedCount, 1);  
   
   compute_new_sid_possible = 0;                       move16(); 

   test();
   if (vad_flag != 0)
   {
      st->dtxHangoverCount = DTX_HANG_CONST;           move16();
   }
   else 
   {  /* non-speech */
      test();
      if (st->dtxHangoverCount == 0)
      {  /* out of decoder analysis hangover  */
         st->decAnaElapsedCount = 0;                   move16();        
         *usedMode = MRDTX;                            move16(); 
         compute_new_sid_possible = 1;                 move16(); 
      }
      else
      { /* in possible analysis hangover */
         st->dtxHangoverCount = sub(st->dtxHangoverCount, 1);
         
         /* decAnaElapsedCount + dtxHangoverCount < DTX_ELAPSED_FRAMES_THRESH */
         test ();
         if (sub(add(st->decAnaElapsedCount, st->dtxHangoverCount),
                 DTX_ELAPSED_FRAMES_THRESH) < 0)
         {
            *usedMode = MRDTX;                         move16(); 
            /* if short time since decoder update, do not add extra HO */            
         }
         /*
          else 
            override VAD and stay in 
            speech mode *usedMode 
            and add extra hangover
         */
      }
   }
   
   return compute_new_sid_possible;
}
