/*
*****************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
*****************************************************************************
*
*      File             : dtx_dec.c
*      Purpose          : Decode comfort noise when in DTX
*
*****************************************************************************
*/
/*
*****************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
*****************************************************************************
*/
#include "dtx_dec.h"
const char dtx_dec_id[] = "@(#)$Id $" dtx_dec_h;
 
/*
*****************************************************************************
*                         INCLUDE FILES
*****************************************************************************
*/
#include <stdlib.h>
#include <stdio.h>
#include "typedef.h"
#include "basic_op.h"
#include "oper_32b.h"
#include "copy.h"
#include "set_zero.h"
#include "mode.h"
#include "log2.h"
#include "lsp_az.h"
#include "pow2.h"
#include "a_refl.h"
#include "b_cn_cod.h"
#include "syn_filt.h"
#include "lsp_lsf.h"
#include "reorder.h"
#include "count.h"
#include "q_plsf_5.tab"
#include "lsp.tab"

/*
*****************************************************************************
*                         LOCAL VARIABLES AND TABLES
*****************************************************************************
*/
#define PN_INITIAL_SEED 0x70816958L   /* Pseudo noise generator seed value  */

/***************************************************
 * Scaling factors for the lsp variability operation *
 ***************************************************/
static const Word16 lsf_hist_mean_scale[M] = {
   20000,
   20000,
   20000,
   20000,
   20000,
   18000,
   16384,
    8192,
       0,
       0
};

/*************************************************
 * level adjustment for different modes Q11      *
 *************************************************/
static const Word16 dtx_log_en_adjust[9] =
{
  -1023, /* MR475 */
   -878, /* MR515 */
   -732, /* MR59  */
   -586, /* MR67  */
   -440, /* MR74  */
   -294, /* MR795 */
   -148, /* MR102 */
      0, /* MR122 */
      0, /* MRDTX */
};

/*
*****************************************************************************
*                         PUBLIC PROGRAM CODE
*****************************************************************************
*/
/*
**************************************************************************
*
*  Function    : dtx_dec_init
*
**************************************************************************
*/ 
int dtx_dec_init (dtx_decState **st)
{
   dtx_decState* s;
   
   if (st == (dtx_decState **) NULL){
      fprintf(stderr, "dtx_dec_init: invalid parameter\n");
      return -1; 
   }
   
   *st = NULL;
   
   /* allocate memory */
   if ((s= (dtx_decState *) malloc(sizeof(dtx_decState))) == NULL){
      fprintf(stderr, "dtx_dec_init: can not malloc state structure\n");
      return -1;
   }
   
   dtx_dec_reset(s);
   *st = s;
   
   return 0;
}
 
/*
**************************************************************************
*
*  Function    : dtx_dec_reset
*
**************************************************************************
*/
int dtx_dec_reset (dtx_decState *st)
{
   int i;

   if (st == (dtx_decState *) NULL){
      fprintf(stderr, "dtx_dec_reset: invalid parameter\n");
      return -1;
   }
   
   st->since_last_sid = 0;
   st->true_sid_period_inv = (1 << 13); 
 
   st->log_en = 3500;  
   st->old_log_en = 3500;
   /* low level noise for better performance in  DTX handover cases*/
   
   st->L_pn_seed_rx = PN_INITIAL_SEED;

   /* Initialize state->lsp [] and state->lsp_old [] */
   Copy(lsp_init_data, &st->lsp[0], M);
   Copy(lsp_init_data, &st->lsp_old[0], M);

   st->lsf_hist_ptr = 0;
   st->log_pg_mean = 0;
   st->log_en_hist_ptr = 0;

   /* initialize decoder lsf history */
   Copy(mean_lsf, &st->lsf_hist[0], M);

   for (i = 1; i < DTX_HIST_SIZE; i++)
   {
      Copy(&st->lsf_hist[0], &st->lsf_hist[M*i], M);
   }
   Set_zero(st->lsf_hist_mean, M*DTX_HIST_SIZE);

   /* initialize decoder log frame energy */ 
   for (i = 0; i < DTX_HIST_SIZE; i++)
   {
      st->log_en_hist[i] = st->log_en;
   }

   st->log_en_adjust = 0;

   st->dtxHangoverCount = DTX_HANG_CONST;
   st->decAnaElapsedCount = 32767;   

   st->sid_frame = 0;       
   st->valid_data = 0;             
   st->dtxHangoverAdded = 0; 
  
   st->dtxGlobalState = DTX;    
   st->data_updated = 0; 
   return 0;
}
 
/*
**************************************************************************
*
*  Function    : dtx_dec_exit
*
**************************************************************************
*/
void dtx_dec_exit (dtx_decState **st)
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
*  Function    : dtx_dec
*                
**************************************************************************
*/
int dtx_dec(
   dtx_decState *st,                /* i/o : State struct                    */
   Word16 mem_syn[],                /* i/o : AMR decoder state               */
   D_plsfState* lsfState,           /* i/o : decoder lsf states              */
   gc_predState* predState,         /* i/o : prediction states               */
   Cb_gain_averageState* averState, /* i/o : CB gain average states          */
   enum DTXStateType new_state,     /* i   : new DTX state                   */
   enum Mode mode,                  /* i   : AMR mode                        */
   Word16 parm[],                   /* i   : Vector of synthesis parameters  */
   Word16 synth[],                  /* o   : synthesised speech              */
   Word16 A_t[]                     /* o   : decoded LP filter in 4 subframes*/
   )
{
   Word16 log_en_index;
   Word16 i, j;
   Word16 int_fac;
   Word32 L_log_en_int;
   Word16 lsp_int[M];
   Word16 log_en_int_e;
   Word16 log_en_int_m;
   Word16 level;
   Word16 acoeff[M + 1];
   Word16 refl[M];
   Word16 pred_err;
   Word16 ex[L_SUBFR];
   Word16 ma_pred_init;
   Word16 log_pg_e, log_pg_m;
   Word16 log_pg;
   Flag negative;
   Word16 lsf_mean;
   Word32 L_lsf_mean;
   Word16 lsf_variab_index;
   Word16 lsf_variab_factor;
   Word16 lsf_int[M];
   Word16 lsf_int_variab[M];
   Word16 lsp_int_variab[M];
   Word16 acoeff_variab[M + 1];

   Word16 lsf[M];
   Word32 L_lsf[M];
   Word16 ptr;
   Word16 tmp_int_length;


   /*  This function is called if synthesis state is not SPEECH 
    *  the globally passed  inputs to this function are 
    * st->sid_frame 
    * st->valid_data 
    * st->dtxHangoverAdded
    * new_state  (SPEECH, DTX, DTX_MUTE)
    */

   test(); test();
   if ((st->dtxHangoverAdded != 0) && 
       (st->sid_frame != 0))
   {
      /* sid_first after dtx hangover period */
      /* or sid_upd after dtxhangover        */

      /* set log_en_adjust to correct value */
      st->log_en_adjust = dtx_log_en_adjust[mode];
          
      ptr = add(st->lsf_hist_ptr, M);                               move16(); 
      test();
      if (sub(ptr, 80) == 0)
      {
         ptr = 0;                                                   move16();
      }
      Copy( &st->lsf_hist[st->lsf_hist_ptr],&st->lsf_hist[ptr],M); 
      
      ptr = add(st->log_en_hist_ptr,1);                             move16();
      test();
      if (sub(ptr, DTX_HIST_SIZE) == 0)
      {
         ptr = 0;                                                   move16();
      }
      move16();
      st->log_en_hist[ptr] = st->log_en_hist[st->log_en_hist_ptr]; /* Q11 */
      
      /* compute mean log energy and lsp *
       * from decoded signal (SID_FIRST) */         
      st->log_en = 0;                                               move16();
      for (i = 0; i < M; i++)
      {
         L_lsf[i] = 0;                                              move16();
      }
      
      /* average energy and lsp */
      for (i = 0; i < DTX_HIST_SIZE; i++)
      {
         st->log_en = add(st->log_en,
                          shr(st->log_en_hist[i],3));
         for (j = 0; j < M; j++)
         {
            L_lsf[j] = L_add(L_lsf[j],
                             L_deposit_l(st->lsf_hist[i * M + j]));
         }
      }
       
      for (j = 0; j < M; j++)
      {
         lsf[j] = extract_l(L_shr(L_lsf[j],3)); /* divide by 8 */  move16();
      }
      
      Lsf_lsp(lsf, st->lsp, M); 

      /* make log_en speech coder mode independent */
      /* added again later before synthesis        */
      st->log_en = sub(st->log_en, st->log_en_adjust);

      /* compute lsf variability vector */
      Copy(st->lsf_hist, st->lsf_hist_mean, 80);

      for (i = 0; i < M; i++)
      {
         L_lsf_mean = 0;                                           move32();
         /* compute mean lsf */
         for (j = 0; j < 8; j++)
         {
            L_lsf_mean = L_add(L_lsf_mean, 
                               L_deposit_l(st->lsf_hist_mean[i+j*M]));
         }
         
         lsf_mean = extract_l(L_shr(L_lsf_mean, 3));               move16();
         /* subtract mean and limit to within reasonable limits  *
          * moreover the upper lsf's are attenuated              */
         for (j = 0; j < 8; j++)
         {
            /* subtract mean */ 
            st->lsf_hist_mean[i+j*M] = 
               sub(st->lsf_hist_mean[i+j*M], lsf_mean);

            /* attenuate deviation from mean, especially for upper lsf's */
            st->lsf_hist_mean[i+j*M] = 
               mult(st->lsf_hist_mean[i+j*M], lsf_hist_mean_scale[i]);

            /* limit the deviation */
            test();
            if (st->lsf_hist_mean[i+j*M] < 0)
            {
               negative = 1;                                        move16();
            }
            else
            {
               negative = 0;                                        move16();
            }
            st->lsf_hist_mean[i+j*M] = abs_s(st->lsf_hist_mean[i+j*M]);

            /* apply soft limit */
            test();
            if (sub(st->lsf_hist_mean[i+j*M], 655) > 0)
            {
               st->lsf_hist_mean[i+j*M] = 
                  add(655, shr(sub(st->lsf_hist_mean[i+j*M], 655), 2));
            }
            
            /* apply hard limit */
            test();
            if (sub(st->lsf_hist_mean[i+j*M], 1310) > 0)
            {
               st->lsf_hist_mean[i+j*M] = 1310;                     move16();
            }
            test();
            if (negative != 0) 
            {
               st->lsf_hist_mean[i+j*M] = -st->lsf_hist_mean[i+j*M];move16();
            }
            
         }
      }
   }
   
   test();
   if (st->sid_frame != 0 )
   {
      /* Set old SID parameters, always shift */
      /* even if there is no new valid_data   */
      Copy(st->lsp, st->lsp_old, M);
      st->old_log_en = st->log_en;                                  move16();

      test();
      if (st->valid_data != 0 )  /* new data available (no CRC) */
      {
         /* Compute interpolation factor, since the division only works *
          * for values of since_last_sid < 32 we have to limit the      *
          * interpolation to 32 frames                                  */
         tmp_int_length = st->since_last_sid;                       move16();
         st->since_last_sid = 0;                                    move16();

         test();
         if (sub(tmp_int_length, 32) > 0)
         {
            tmp_int_length = 32;                                    move16();
         }
         test();
         if (sub(tmp_int_length, 2) >= 0)
         {
            move16();
            st->true_sid_period_inv = div_s(1 << 10, shl(tmp_int_length, 10)); 
         }
         else
         {
            st->true_sid_period_inv = 1 << 14; /* 0.5 it Q15 */     move16();
         }
         
         Init_D_plsf_3(lsfState, parm[0]);  /* temporay initialization */ 
         D_plsf_3(lsfState, MRDTX, 0, &parm[1], st->lsp);
         Set_zero(lsfState->past_r_q, M);   /* reset for next speech frame */ 

         log_en_index = parm[4];                                    move16();
         /* Q11 and divide by 4 */
         st->log_en = shl(log_en_index, (11 - 2));                  move16();
         
         /* Subtract 2.5 in Q11 */
         st->log_en = sub(st->log_en, (2560 * 2));
         
         /* Index 0 is reserved for silence */
         test();
         if (log_en_index == 0)
         {
            st->log_en = MIN_16;                                    move16();
         }
         
         /* no interpolation at startup after coder reset        */
         /* or when SID_UPD has been received right after SPEECH */
         test(); test();
         if ((st->data_updated == 0) ||
             (sub(st->dtxGlobalState, SPEECH) == 0)
             ) 
         {
            Copy(st->lsp, st->lsp_old, M);
            st->old_log_en = st->log_en;                            move16();
         }         
      } /* endif valid_data */

      /* initialize gain predictor memory of other modes */       
      ma_pred_init = sub(shr(st->log_en,1), 9000);                  move16();
      test();
      if (ma_pred_init > 0)
      {                   
         ma_pred_init = 0;                                          move16();  
      }      
      test();
      if (sub(ma_pred_init, -14436) < 0)
      {
         ma_pred_init = -14436;                                     move16();
      }
      
      predState->past_qua_en[0] = ma_pred_init;                     move16();
      predState->past_qua_en[1] = ma_pred_init;                     move16();
      predState->past_qua_en[2] = ma_pred_init;                     move16();
      predState->past_qua_en[3] = ma_pred_init;                     move16();

      /* past_qua_en for other modes than MR122 */      
      ma_pred_init = mult(5443, ma_pred_init); 
      /* scale down by factor 20*log10(2) in Q15 */
      predState->past_qua_en_MR122[0] = ma_pred_init;               move16();
      predState->past_qua_en_MR122[1] = ma_pred_init;               move16();
      predState->past_qua_en_MR122[2] = ma_pred_init;               move16();
      predState->past_qua_en_MR122[3] = ma_pred_init;               move16();
   } /* endif sid_frame */
   
   /* CN generation */
   /* recompute level adjustment factor Q11             *
    * st->log_en_adjust = 0.9*st->log_en_adjust +       *
    *                     0.1*dtx_log_en_adjust[mode]); */
   move16();
   st->log_en_adjust = add(mult(st->log_en_adjust, 29491),
                           shr(mult(shl(dtx_log_en_adjust[mode],5),3277),5));

   /* Interpolate SID info */
   int_fac = shl(add(1,st->since_last_sid), 10); /* Q10 */                 move16();
   int_fac = mult(int_fac, st->true_sid_period_inv); /* Q10 * Q15 -> Q10 */
   
   /* Maximize to 1.0 in Q10 */
   test();
   if (sub(int_fac, 1024) > 0)
   {
      int_fac = 1024;                                               move16();
   }
   int_fac = shl(int_fac, 4); /* Q10 -> Q14 */
   
   L_log_en_int = L_mult(int_fac, st->log_en); /* Q14 * Q11->Q26 */ move32();
   for(i = 0; i < M; i++)
   {
      lsp_int[i] = mult(int_fac, st->lsp[i]);/* Q14 * Q15 -> Q14 */ move16();
   }
   
   int_fac = sub(16384, int_fac); /* 1-k in Q14 */                  move16();

   /* (Q14 * Q11 -> Q26) + Q26 -> Q26 */
   L_log_en_int = L_mac(L_log_en_int, int_fac, st->old_log_en);
   for(i = 0; i < M; i++)
   {
      /* Q14 + (Q14 * Q15 -> Q14) -> Q14 */
      lsp_int[i] = add(lsp_int[i], mult(int_fac, st->lsp_old[i]));  move16();
      lsp_int[i] = shl(lsp_int[i], 1); /* Q14 -> Q15 */             move16();
   }
   
   /* compute the amount of lsf variability */
   lsf_variab_factor = sub(st->log_pg_mean,2457); /* -0.6 in Q12 */ move16();
   /* *0.3 Q12*Q15 -> Q12 */
   lsf_variab_factor = sub(4096, mult(lsf_variab_factor, 9830)); 

   /* limit to values between 0..1 in Q12 */ 
   test();
   if (sub(lsf_variab_factor, 4096) > 0)
   {
      lsf_variab_factor = 4096;                                     move16();
   }
   test();
   if (lsf_variab_factor < 0)
   {
      lsf_variab_factor = 0;                                        move16(); 
   }
   lsf_variab_factor = shl(lsf_variab_factor, 3); /* -> Q15 */      move16();

   /* get index of vector to do variability with */
   lsf_variab_index = pseudonoise(&st->L_pn_seed_rx, 3);            move16();

   /* convert to lsf */
   Lsp_lsf(lsp_int, lsf_int, M);

   /* apply lsf variability */
   Copy(lsf_int, lsf_int_variab, M);
   for(i = 0; i < M; i++)
   {
      move16();
      lsf_int_variab[i] = add(lsf_int_variab[i], 
                              mult(lsf_variab_factor,
                                   st->lsf_hist_mean[i+lsf_variab_index*M]));
   }

   /* make sure that LSP's are ordered */
   Reorder_lsf(lsf_int, LSF_GAP, M);
   Reorder_lsf(lsf_int_variab, LSF_GAP, M);

   /* copy lsf to speech decoders lsf state */
   Copy(lsf_int, lsfState->past_lsf_q, M);

   /* convert to lsp */
   Lsf_lsp(lsf_int, lsp_int, M);
   Lsf_lsp(lsf_int_variab, lsp_int_variab, M);

   /* Compute acoeffs Q12 acoeff is used for level    * 
    * normalization and postfilter, acoeff_variab is  *
    * used for synthesis filter                       *
    * by doing this we make sure that the level       *
    * in high frequenncies does not jump up and down  */

   Lsp_Az(lsp_int, acoeff);
   Lsp_Az(lsp_int_variab, acoeff_variab);
   
   /* For use in postfilter */
   Copy(acoeff, &A_t[0],           M + 1);
   Copy(acoeff, &A_t[M + 1],       M + 1);
   Copy(acoeff, &A_t[2 * (M + 1)], M + 1);
   Copy(acoeff, &A_t[3 * (M + 1)], M + 1);
   
   /* Compute reflection coefficients Q15 */
   A_Refl(&acoeff[1], refl);
   
   /* Compute prediction error in Q15 */
   pred_err = MAX_16; /* 0.99997 in Q15 */                          move16();
   for (i = 0; i < M; i++)
   { 
      pred_err = mult(pred_err, sub(MAX_16, mult(refl[i], refl[i])));
   }

   /* compute logarithm of prediction gain */   
   Log2(L_deposit_l(pred_err), &log_pg_e, &log_pg_m);
   
   /* convert exponent and mantissa to Word16 Q12 */
   log_pg = shl(sub(log_pg_e,15), 12);  /* Q12 */                   move16();
   log_pg = shr(sub(0,add(log_pg, shr(log_pg_m, 15-12))), 1);       move16();
   st->log_pg_mean = add(mult(29491,st->log_pg_mean),
                         mult(3277, log_pg));                       move16();

   /* Compute interpolated log energy */
   L_log_en_int = L_shr(L_log_en_int, 10); /* Q26 -> Q16 */         move32();

   /* Add 4 in Q16 */
   L_log_en_int = L_add(L_log_en_int, 4 * 65536L);                  move32();

   /* subtract prediction gain */
   L_log_en_int = L_sub(L_log_en_int, L_shl(L_deposit_l(log_pg), 4));move32();

   /* adjust level to speech coder mode */
   L_log_en_int = L_add(L_log_en_int, 
                        L_shl(L_deposit_l(st->log_en_adjust), 5));  move32();
       
   log_en_int_e = extract_h(L_log_en_int);                    move16();
   move16();
   log_en_int_m = extract_l(L_shr(L_sub(L_log_en_int, 
                                        L_deposit_h(log_en_int_e)), 1));
   level = extract_l(Pow2(log_en_int_e, log_en_int_m)); /* Q4 */ move16();
   
   for (i = 0; i < 4; i++)
   {             
      /* Compute innovation vector */
      build_CN_code(&st->L_pn_seed_rx, ex);
      for (j = 0; j < L_SUBFR; j++)
      {
         ex[j] = mult(level, ex[j]);                                move16();
      }
      /* Synthesize */
      Syn_filt(acoeff_variab, ex, &synth[i * L_SUBFR], L_SUBFR, 
               mem_syn, 1);
      
   } /* next i */
   
   /* reset codebook averaging variables */ 
   averState->hangVar = 20;                                         move16();
   averState->hangCount = 0;                                        move16();
    
   test();
   if (sub(new_state, DTX_MUTE) == 0)
   {
      /* mute comfort noise as it has been quite a long time since  
       * last SID update  was performed                            */
      
      tmp_int_length = st->since_last_sid;                          move16();
      test();
      if (sub(tmp_int_length, 32) > 0)
      {
         tmp_int_length = 32;                                       move16();
      }
      
      /* safety guard against division by zero */
      test();
      if(tmp_int_length <= 0) {
         tmp_int_length = 8;                                       move16();
      }      
      
      move16();
      st->true_sid_period_inv = div_s(1 << 10, shl(tmp_int_length, 10)); 

      st->since_last_sid = 0;                                       move16();
      Copy(st->lsp, st->lsp_old, M);
      st->old_log_en = st->log_en;                                  move16();
      /* subtract 1/8 in Q11 i.e -6/8 dB */
      st->log_en = sub(st->log_en, 256);                            move16();  
   }

   /* reset interpolation length timer 
    * if data has been updated.        */
   test(); test(); test(); test();
   if ((st->sid_frame != 0) && 
       ((st->valid_data != 0) || 
        ((st->valid_data == 0) &&  (st->dtxHangoverAdded) != 0))) 
   {
      st->since_last_sid =  0;                                      move16();
      st->data_updated = 1;                                         move16();
   }
         
   return 0;
}

void dtx_dec_activity_update(dtx_decState *st,
                             Word16 lsf[],
                             Word16 frame[])
{
   Word16 i;

   Word32 L_frame_en;
   Word16 log_en_e, log_en_m, log_en;

   /* update lsp history */
   st->lsf_hist_ptr = add(st->lsf_hist_ptr,M);                     move16();
   test();
   if (sub(st->lsf_hist_ptr, 80) == 0)
   {
      st->lsf_hist_ptr = 0;                                        move16();
   }
   Copy(lsf, &st->lsf_hist[st->lsf_hist_ptr], M); 

   /* compute log energy based on frame energy */
   L_frame_en = 0;     /* Q0 */                                    move32();
   for (i=0; i < L_FRAME; i++)
   {
      L_frame_en = L_mac(L_frame_en, frame[i], frame[i]); 
   }
   Log2(L_frame_en, &log_en_e, &log_en_m);
   
   /* convert exponent and mantissa to Word16 Q10 */
   log_en = shl(log_en_e, 10);  /* Q10 */                          
   log_en = add(log_en, shr(log_en_m, 15-10));                      
   
   /* divide with L_FRAME i.e subtract with log2(L_FRAME) = 7.32193 */
   log_en = sub(log_en, 7497+1024);                                
   
   /* insert into log energy buffer, no division by two as  *
    * log_en in decoder is Q11                              */
   st->log_en_hist_ptr = add(st->log_en_hist_ptr, 1);
   test();
   if (sub(st->log_en_hist_ptr, DTX_HIST_SIZE) == 0)
   {
      st->log_en_hist_ptr = 0;                                     move16();
   }
   st->log_en_hist[st->log_en_hist_ptr] = log_en; /* Q11 */        move16();
}

/*   
     Table of new SPD synthesis states 
     
                           |     previous SPD_synthesis_state
     Incoming              |  
     frame_type            | SPEECH       | DTX           | DTX_MUTE   
     ---------------------------------------------------------------
     RX_SPEECH_GOOD ,      |              |               |
     RX_SPEECH_PR_DEGRADED | SPEECH       | SPEECH        | SPEECH 
     ----------------------------------------------------------------       
     RX_SPEECH_BAD,        | SPEECH       | DTX           | DTX_MUTE
     ----------------------------------------------------------------
     RX_SID_FIRST,         | DTX          | DTX/(DTX_MUTE)| DTX_MUTE  
     ----------------------------------------------------------------
     RX_SID_UPDATE,        | DTX          | DTX           | DTX
     ----------------------------------------------------------------
     RX_SID_BAD,           | DTX          | DTX/(DTX_MUTE)| DTX_MUTE
     ----------------------------------------------------------------
     RX_NO_DATA            | SPEECH       | DTX/(DTX_MUTE)| DTX_MUTE
                           |(class2 garb.)|               |
     ----------------------------------------------------------------
     RX_ONSET              | SPEECH       | DTX/(DTX_MUTE)| DTX_MUTE
                           |(class2 garb.)|               |
     ----------------------------------------------------------------
*/

enum DTXStateType rx_dtx_handler(
                      dtx_decState *st,           /* i/o : State struct     */
                      enum RXFrameType frame_type /* i   : Frame type       */ 
                      )
{
   enum DTXStateType newState;
   enum DTXStateType encState;

   /* DTX if SID frame or previously in DTX{_MUTE} and (NO_RX OR BAD_SPEECH) */
   test(); test(); test();
   test(); test(); test();
   test(); test();   
   if ((sub(frame_type, RX_SID_FIRST) == 0)   ||
       (sub(frame_type, RX_SID_UPDATE) == 0)  ||
       (sub(frame_type, RX_SID_BAD) == 0)     ||
       (((sub(st->dtxGlobalState, DTX) == 0) ||
         (sub(st->dtxGlobalState, DTX_MUTE) == 0)) && 
        ((sub(frame_type, RX_NO_DATA) == 0) ||
         (sub(frame_type, RX_SPEECH_BAD) == 0) || 
         (sub(frame_type, RX_ONSET) == 0))))
   {
      newState = DTX;                                              move16();

      /* stay in mute for these input types */
      test(); test(); test(); test(); test();
      if ((sub(st->dtxGlobalState, DTX_MUTE) == 0) &&
          ((sub(frame_type, RX_SID_BAD) == 0) ||
           (sub(frame_type, RX_SID_FIRST) ==  0) ||
           (sub(frame_type, RX_ONSET) ==  0) ||
           (sub(frame_type, RX_NO_DATA) == 0)))
      {
         newState = DTX_MUTE;                                      move16();
      }

      /* evaluate if noise parameters are too old                     */
      /* since_last_sid is reset when CN parameters have been updated */
      st->since_last_sid = add(st->since_last_sid, 1);             move16();

      /* no update of sid parameters in DTX for a long while */
      /* Due to the delayed update of  st->since_last_sid counter
         SID_UPDATE frames need to be handled separately to avoid
         entering DTX_MUTE for late SID_UPDATE frames
         */
      test(); test(); logic16();
      if((sub(frame_type, RX_SID_UPDATE) != 0) &&
         (sub(st->since_last_sid, DTX_MAX_EMPTY_THRESH) > 0))
      {
         newState = DTX_MUTE;                                      move16();
      }
   }
   else 
   {
      newState = SPEECH;                                           move16();
      st->since_last_sid = 0;                                      move16();
   }
   
   /* 
      reset the decAnaElapsed Counter when receiving CNI data the first  
      time, to robustify counter missmatch after handover
      this might delay the bwd CNI analysis in the new decoder slightly.
   */    
   test(); test();
   if ((st->data_updated == 0) &&
       (sub(frame_type, RX_SID_UPDATE) == 0))
   {
      st->decAnaElapsedCount = 0;                                  move16();
   }

   /* update the SPE-SPD DTX hangover synchronization */
   /* to know when SPE has added dtx hangover         */
   st->decAnaElapsedCount = add(st->decAnaElapsedCount, 1);        move16();
   st->dtxHangoverAdded = 0;                                       move16();
   
   test(); test(); test(); test(); test();
   if ((sub(frame_type, RX_SID_FIRST) == 0)  ||
       (sub(frame_type, RX_SID_UPDATE) == 0) ||
       (sub(frame_type, RX_SID_BAD) == 0)    ||
       (sub(frame_type, RX_ONSET) == 0)      ||
       (sub(frame_type, RX_NO_DATA) == 0))
   {
      encState = DTX;                                              move16();
      
      /*         
         In frame errors simulations RX_NO_DATA may occasionally mean that
         a speech packet was probably sent by the encoder,
         the assumed _encoder_ state should be SPEECH in such cases.
      */
      
      test(); logic16(); 
      if((sub(frame_type, RX_NO_DATA) == 0) &&
         (sub(newState, SPEECH) == 0)) 
      {
         encState = SPEECH;                                       move16();
      }
      
      
      /* Note on RX_ONSET operation differing from RX_NO_DATA operation:
         If a  RX_ONSET is received in the decoder (by "accident")
         it is still most likely that the encoder  state
         for the "ONSET frame" was DTX.
      */      
   }
   else 
   {
      encState = SPEECH;                                           move16();
   }
 
   test();
   if (sub(encState, SPEECH) == 0)
   {
      st->dtxHangoverCount = DTX_HANG_CONST;                       move16();
   }
   else
   {
      test();
      if (sub(st->decAnaElapsedCount, DTX_ELAPSED_FRAMES_THRESH) > 0)
      {
         st->dtxHangoverAdded = 1;                                 move16();
         st->decAnaElapsedCount = 0;                               move16();
         st->dtxHangoverCount = 0;                                 move16();
      }
      else if (test(), st->dtxHangoverCount == 0)
      {
         st->decAnaElapsedCount = 0;                               move16();
      }
      else
      {
         st->dtxHangoverCount = sub(st->dtxHangoverCount, 1);      move16();
      }
   }
   
   if (sub(newState, SPEECH) != 0)
   {
      /* DTX or DTX_MUTE
       * CN data is not in a first SID, first SIDs are marked as SID_BAD 
       *  but will do backwards analysis if a hangover period has been added
       *  according to the state machine above 
       */
      
      st->sid_frame = 0;                                           move16();
      st->valid_data = 0;                                          move16();
            
      test(); 
      if (sub(frame_type, RX_SID_FIRST) == 0)
      {
         st->sid_frame = 1;                                        move16();
      }
      else if (test(), sub(frame_type, RX_SID_UPDATE) == 0)
      {
         st->sid_frame = 1;                                        move16();
         st->valid_data = 1;                                       move16();
      }
      else if (test(), sub(frame_type, RX_SID_BAD) == 0)
      {
         st->sid_frame = 1;                                        move16();
         st->dtxHangoverAdded = 0; /* use old data */              move16();
      } 
   }

   return newState; 
   /* newState is used by both SPEECH AND DTX synthesis routines */ 
}
