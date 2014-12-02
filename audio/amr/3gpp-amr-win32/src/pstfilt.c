/*************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : pstfilt.c
*      Purpose          : Performs adaptive postfiltering on the synthesis
*                       : speech
*
********************************************************************************
*/

/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "pstfilt.h"
const char pstfilt_id[] = "@(#)$Id $" pstfilt_h;
 
/*
********************************************************************************
*                         INCLUDE FILES
********************************************************************************
*/
#include <stdlib.h>
#include <stdio.h>
#include "typedef.h"
#include "mode.h"
#include "basic_op.h"
#include "set_zero.h"
#include "weight_a.h"
#include "residu.h"
#include "copy.h"
#include "syn_filt.h"
#include "preemph.h"
#include "count.h"
#include "cnst.h"

/*
********************************************************************************
*                         LOCAL VARIABLES AND TABLES
********************************************************************************
*/
/*---------------------------------------------------------------*
 *    Postfilter constant parameters (defined in "cnst.h")       *
 *---------------------------------------------------------------*
 *   L_FRAME     : Frame size.                                   *
 *   L_SUBFR     : Sub-frame size.                               *
 *   M           : LPC order.                                    *
 *   MP1         : LPC order+1                                   *
 *   MU          : Factor for tilt compensation filter           *
 *   AGC_FAC     : Factor for automatic gain control             *
 *---------------------------------------------------------------*/

#define L_H 22  /* size of truncated impulse response of A(z/g1)/A(z/g2) */

/* Spectral expansion factors */
static const Word16 gamma3_MR122[M] = {
  22938, 16057, 11240, 7868, 5508,
  3856, 2699, 1889, 1322, 925
};

static const Word16 gamma3[M] = {
  18022, 9912, 5451, 2998, 1649, 907, 499, 274, 151, 83
};

static const Word16 gamma4_MR122[M] = {
  24576, 18432, 13824, 10368, 7776,
  5832, 4374, 3281, 2461, 1846
};

static const Word16 gamma4[M] = {
  22938, 16057, 11240, 7868, 5508, 3856, 2699, 1889, 1322, 925
};

/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/

/*************************************************************************
*
*  Function:   Post_Filter_init
*  Purpose:    Allocates memory for filter structure and initializes
*              state memory
*
**************************************************************************
*/
int Post_Filter_init (Post_FilterState **state)
{
  Post_FilterState* s;

  if (state == (Post_FilterState **) NULL){
      fprintf(stderr, "Post_Filter_init: invalid parameter\n");
      return -1;
  }
  *state = NULL;

  /* allocate memory */
  if ((s= (Post_FilterState *) malloc(sizeof(Post_FilterState))) == NULL){
      fprintf(stderr, "Post_Filter_init: can not malloc state structure\n");
      return -1;
  }
  s->preemph_state = NULL;
  s->agc_state = NULL;

  if (preemphasis_init(&s->preemph_state) || agc_init(&s->agc_state)) {
      Post_Filter_exit(&s);
      return -1;
  }
      
  Post_Filter_reset(s);
  *state = s;
  
  return 0;
}

/*************************************************************************
*
*  Function:   Post_Filter_reset
*  Purpose:    Initializes state memory to zero
*
**************************************************************************
*/
int Post_Filter_reset (Post_FilterState *state)
{
  if (state == (Post_FilterState *) NULL){
      fprintf(stderr, "Post_Filter_reset: invalid parameter\n");
      return -1;
  }
  
  Set_zero (state->mem_syn_pst, M);
  Set_zero (state->res2, L_SUBFR);
  Set_zero (state->synth_buf, L_FRAME + M);
  agc_reset(state->agc_state);
  preemphasis_reset(state->preemph_state);

  return 0;
}

/*************************************************************************
*
*  Function:   Post_Filter_exit
*  Purpose:    The memory used for state memory is freed
*
**************************************************************************
*/
void Post_Filter_exit (Post_FilterState **state)
{
  if (state == NULL || *state == NULL)
      return;

  agc_exit(&(*state)->agc_state);
  preemphasis_exit(&(*state)->preemph_state);
  
  /* deallocate memory */
  free(*state);
  *state = NULL;
  
  return;
}

/*
**************************************************************************
*  Function:  Post_Filter
*  Purpose:   postfiltering of synthesis speech.
*  Description:
*      The postfiltering process is described as follows:
*
*          - inverse filtering of syn[] through A(z/0.7) to get res2[]
*          - tilt compensation filtering; 1 - MU*k*z^-1
*          - synthesis filtering through 1/A(z/0.75)
*          - adaptive gain control
*
**************************************************************************
*/
int Post_Filter (
    Post_FilterState *st, /* i/o : post filter states                        */
    enum Mode mode,       /* i   : AMR mode                                  */
    Word16 *syn,          /* i/o : synthesis speech (postfiltered is output) */
    Word16 *Az_4          /* i   : interpolated LPC parameters in all subfr. */
)
{
    /*-------------------------------------------------------------------*
     *           Declaration of parameters                               *
     *-------------------------------------------------------------------*/

    Word16 Ap3[MP1], Ap4[MP1];  /* bandwidth expanded LP parameters */
    Word16 *Az;                 /* pointer to Az_4:                 */
                                /*  LPC parameters in each subframe */
    Word16 i_subfr;             /* index for beginning of subframe  */
    Word16 h[L_H];

    Word16 i;
    Word16 temp1, temp2;
    Word32 L_tmp;
    Word16 *syn_work = &st->synth_buf[M]; move16 ();
    

    /*-----------------------------------------------------*
     * Post filtering                                      *
     *-----------------------------------------------------*/

    Copy (syn, syn_work , L_FRAME);

    Az = Az_4;

    for (i_subfr = 0; i_subfr < L_FRAME; i_subfr += L_SUBFR)
    {
       /* Find weighted filter coefficients Ap3[] and ap[4] */

       test (); test ();
       if (sub(mode, MR122) == 0 || sub(mode, MR102) == 0)
       {
          Weight_Ai (Az, gamma3_MR122, Ap3);
          Weight_Ai (Az, gamma4_MR122, Ap4);
       }
       else
       {
          Weight_Ai (Az, gamma3, Ap3);
          Weight_Ai (Az, gamma4, Ap4);
       }
       
       /* filtering of synthesis speech by A(z/0.7) to find res2[] */
       
       Residu (Ap3, &syn_work[i_subfr], st->res2, L_SUBFR);
       
       /* tilt compensation filter */
       
       /* impulse response of A(z/0.7)/A(z/0.75) */

       Copy (Ap3, h, M + 1);
       Set_zero (&h[M + 1], L_H - M - 1);
       Syn_filt (Ap4, h, h, L_H, &h[M + 1], 0);
        
       /* 1st correlation of h[] */
       
       L_tmp = L_mult (h[0], h[0]);
       for (i = 1; i < L_H; i++)
       {
          L_tmp = L_mac (L_tmp, h[i], h[i]);
       }
       temp1 = extract_h (L_tmp);
       
       L_tmp = L_mult (h[0], h[1]);
       for (i = 1; i < L_H - 1; i++)
       {
          L_tmp = L_mac (L_tmp, h[i], h[i + 1]);
       }
       temp2 = extract_h (L_tmp);
       
       test (); 
       if (temp2 <= 0)
       {
          temp2 = 0;          move16 (); 
       }
       else
       {
          temp2 = mult (temp2, MU);
          temp2 = div_s (temp2, temp1);
       }
       
       preemphasis (st->preemph_state, st->res2, temp2, L_SUBFR);
       
       /* filtering through  1/A(z/0.75) */
       
       Syn_filt (Ap4, st->res2, &syn[i_subfr], L_SUBFR, st->mem_syn_pst, 1);
       
       /* scale output to input */

       agc (st->agc_state, &syn_work[i_subfr], &syn[i_subfr],
            AGC_FAC, L_SUBFR);
       
       Az += MP1;
    }
    
    /* update syn_work[] buffer */
    
    Copy (&syn_work[L_FRAME - M], &syn_work[-M], M);
    
    return 0;
}
