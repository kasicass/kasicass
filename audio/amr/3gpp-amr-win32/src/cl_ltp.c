/*
*****************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
*****************************************************************************
*
*      File             : cl_ltp.c
*
*****************************************************************************
*/

/*
*****************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
*****************************************************************************
*/
#include "cl_ltp.h"
const char cl_ltp_id[] = "@(#)$Id $" cl_ltp_h;
 
/*
*****************************************************************************
*                         INCLUDE FILES
*****************************************************************************
*/
#include <stdlib.h>
#include <stdio.h>
#include "typedef.h"
#include "basic_op.h"
#include "count.h"
#include "oper_32b.h"
#include "cnst.h"
#include "convolve.h"
#include "g_pitch.h"
#include "pred_lt.h"
#include "pitch_fr.h"
#include "enc_lag3.h"
#include "enc_lag6.h"
#include "q_gain_p.h"
#include "ton_stab.h"

/*
*****************************************************************************
*                         LOCAL VARIABLES AND TABLES
*****************************************************************************
*/

/*
*****************************************************************************
*                         PUBLIC PROGRAM CODE
*****************************************************************************
*/
/*************************************************************************
*
*  Function:   cl_ltp_init
*  Purpose:    Allocates state memory and initializes state memory
*
**************************************************************************
*/
int cl_ltp_init (clLtpState **state)
{
    clLtpState* s;
    
    if (state == (clLtpState **) NULL){
        fprintf(stderr, "cl_ltp_init: invalid parameter\n");
        return -1;
    }
    *state = NULL;
    
    /* allocate memory */
    if ((s= (clLtpState *) malloc(sizeof(clLtpState))) == NULL){
        fprintf(stderr, "cl_ltp_init: can not malloc state structure\n");
        return -1;
  }
    
    /* init the sub state */
    if (Pitch_fr_init(&s->pitchSt)) {
        cl_ltp_exit(&s);
        return -1;
    }
    
    cl_ltp_reset(s);
    
    *state = s;
    
    return 0;
}
 
/*************************************************************************
*
*  Function:   cl_ltp_reset
*  Purpose:    Initializes state memory to zero
*
**************************************************************************
*/
int cl_ltp_reset (clLtpState *state)
{
    if (state == (clLtpState *) NULL){
        fprintf(stderr, "cl_ltp_reset: invalid parameter\n");
        return -1;
    }
    
    /* Reset pitch search states */
    Pitch_fr_reset (state->pitchSt);
    
    return 0;
}

/*************************************************************************
*
*  Function:   cl_ltp_exit
*  Purpose:    The memory used for state memory is freed
*
**************************************************************************
*/
void cl_ltp_exit (clLtpState **state)
{
    if (state == NULL || *state == NULL)
        return;

    /* dealloc members */
    Pitch_fr_exit(&(*state)->pitchSt);
    
    /* deallocate memory */
    free(*state);
    *state = NULL;
    
    return;
}

/*************************************************************************
*
*  Function:   cl_ltp
*  Purpose:    closed-loop fractional pitch search
*
**************************************************************************
*/
int cl_ltp (
    clLtpState *clSt,    /* i/o : State struct                              */
    tonStabState *tonSt, /* i/o : State struct                              */
    enum Mode mode,      /* i   : coder mode                                */
    Word16 frameOffset,  /* i   : Offset to subframe                        */
    Word16 T_op[],       /* i   : Open loop pitch lags                      */
    Word16 *h1,          /* i   : Impulse response vector               Q12 */
    Word16 *exc,         /* i/o : Excitation vector                      Q0 */
    Word16 res2[],       /* i/o : Long term prediction residual          Q0 */
    Word16 xn[],         /* i   : Target vector for pitch search         Q0 */
    Word16 lsp_flag,     /* i   : LSP resonance flag                        */
    Word16 xn2[],        /* o   : Target vector for codebook search      Q0 */
    Word16 y1[],         /* o   : Filtered adaptive excitation           Q0 */
    Word16 *T0,          /* o   : Pitch delay (integer part)                */
    Word16 *T0_frac,     /* o   : Pitch delay (fractional part)             */
    Word16 *gain_pit,    /* o   : Pitch gain                            Q14 */
    Word16 g_coeff[],    /* o   : Correlations between xn, y1, & y2         */
    Word16 **anap,       /* o   : Analysis parameters                       */
    Word16 *gp_limit     /* o   : pitch gain limit                          */
)
{
    Word16 i;
    Word16 index;
    Word32 L_temp;     /* temporarily variable */
    Word16 resu3;      /* flag for upsample resolution */
    Word16 gpc_flag;
    
   /*----------------------------------------------------------------------*
    *                 Closed-loop fractional pitch search                  *
    *----------------------------------------------------------------------*/
   *T0 = Pitch_fr(clSt->pitchSt,
                  mode, T_op, exc, xn, h1, 
                  L_SUBFR, frameOffset,
                  T0_frac, &resu3, &index); move16 ();
   
   *(*anap)++ = index;                              move16 ();
   
   /*-----------------------------------------------------------------*
    *   - find unity gain pitch excitation (adapitve codebook entry)  *
    *     with fractional interpolation.                              *
    *   - find filtered pitch exc. y1[]=exc[] convolve with h1[])     *
    *   - compute pitch gain and limit between 0 and 1.2              *
    *   - update target vector for codebook search                    *
    *   - find LTP residual.                                          *
    *-----------------------------------------------------------------*/
   
   Pred_lt_3or6(exc, *T0, *T0_frac, L_SUBFR, resu3);
   
   Convolve(exc, h1, y1, L_SUBFR);
   
   /* gain_pit is Q14 for all modes */
   *gain_pit = G_pitch(mode, xn, y1, g_coeff, L_SUBFR); move16 ();

   
   /* check if the pitch gain should be limit due to resonance in LPC filter */
   gpc_flag = 0;                                        move16 ();
   *gp_limit = MAX_16;                                  move16 ();
   test (); test ();
   if ((lsp_flag != 0) &&
       (sub(*gain_pit, GP_CLIP) > 0))
   {
       gpc_flag = check_gp_clipping(tonSt, *gain_pit);  move16 ();
   }

   /* special for the MR475, MR515 mode; limit the gain to 0.85 to */
   /* cope with bit errors in the decoder in a better way.         */
   test (); test (); 
   if ((sub (mode, MR475) == 0) || (sub (mode, MR515) == 0)) {
      test ();
      if ( sub (*gain_pit, 13926) > 0) {
         *gain_pit = 13926;   /* 0.85 in Q14 */    move16 ();
      }

      test ();
      if (gpc_flag != 0) {
          *gp_limit = GP_CLIP;                     move16 ();
      }
   }
   else
   {
       test ();
       if (gpc_flag != 0)
       {
           *gp_limit = GP_CLIP;                    move16 ();
           *gain_pit = GP_CLIP;                    move16 ();
       }           
       /* For MR122, gain_pit is quantized here and not in gainQuant */
       if (test (), sub(mode, MR122)==0)
       {
           *(*anap)++ = q_gain_pitch(MR122, *gp_limit, gain_pit,
                                     NULL, NULL);
                                                   move16 ();
       }
   }

   /* update target vector und evaluate LTP residual */
   for (i = 0; i < L_SUBFR; i++) {
       L_temp = L_mult(y1[i], *gain_pit);
       L_temp = L_shl(L_temp, 1);
       xn2[i] = sub(xn[i], extract_h(L_temp));     move16 ();
      
       L_temp = L_mult(exc[i], *gain_pit);
       L_temp = L_shl(L_temp, 1);
       res2[i] = sub(res2[i], extract_h(L_temp));  move16 ();
   }
   
   return 0;
}
