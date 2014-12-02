/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : ec_gains.c
*      Purpose:         : Error concealment for pitch and codebook gains
*
********************************************************************************
*/
 
 
/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "ec_gains.h"
const char ec_gains_id[] = "@(#)$Id $" ec_gains_h;
 
/*
********************************************************************************
*                         INCLUDE FILES
********************************************************************************
*/
#include <stdlib.h>
#include <stdio.h>
#include "typedef.h"
#include "basic_op.h"
#include "oper_32b.h"
#include "count.h"
#include "cnst.h"
#include "gmed_n.h"
#include "gc_pred.h"

/*
********************************************************************************
*                         LOCAL VARIABLES AND TABLES
********************************************************************************
*/
#include "gains.tab"

/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
/*
**************************************************************************
*
*  Function    : ec_gain_code_init
*  Purpose     : Allocates memory and initializes state variables
*
**************************************************************************
*/
int ec_gain_code_init (ec_gain_codeState **state)
{
  ec_gain_codeState* s;
 
  if (state == (ec_gain_codeState **) NULL){
      fprintf(stderr, "ec_gain_code_init: invalid parameter\n");
      return -1;
  }
  *state = NULL;
 
  /* allocate memory */
  if ((s= (ec_gain_codeState *) malloc(sizeof(ec_gain_codeState))) == NULL){
      fprintf(stderr, "ec_gain_code_init: can not malloc state structure\n");
      return -1;
  }

  ec_gain_code_reset(s);
  *state = s;
  
  return 0;
}
 
/*
**************************************************************************
*
*  Function    : ec_gain_code_reset
*  Purpose     : Resets state memory
*
**************************************************************************
*/
int ec_gain_code_reset (ec_gain_codeState *state)
{
  Word16 i;
  
  if (state == (ec_gain_codeState *) NULL){
      fprintf(stderr, "ec_gain_code_reset: invalid parameter\n");
      return -1;
  }

  for ( i = 0; i < 5; i++)
      state->gbuf[i] = 1;
  state->past_gain_code = 0;
  state->prev_gc = 1;       

  return 0;
}
 
/*
**************************************************************************
*
*  Function    : ec_gain_code_exit
*  Purpose     : The memory used for state memory is freed
*
**************************************************************************
*/
void ec_gain_code_exit (ec_gain_codeState **state)
{
  if (state == NULL || *state == NULL)
      return;

  /* deallocate memory */
  free(*state);
  *state = NULL;
  
  return;
}

/*
**************************************************************************
*
*  Function    : ec_gain_code
*  Purpose     : conceal the codebook gain
*                Call this function only in BFI (instead of normal gain
*                decoding function)
*
**************************************************************************
*/
void ec_gain_code (
    ec_gain_codeState *st,    /* i/o : State struct                     */
    gc_predState *pred_state, /* i/o : MA predictor state               */
    Word16 state,             /* i   : state of the state machine       */
    Word16 *gain_code         /* o   : decoded innovation gain          */
)
{
    static const Word16 cdown[7] =
    {
        32767, 32112, 32112, 32112,
        32112, 32112, 22937
    };

    Word16 tmp;
    Word16 qua_ener_MR122;
    Word16 qua_ener;
    
    /* calculate median of last five gain values */
    tmp = gmed_n (st->gbuf,5);                                 move16 ();

    /* new gain = minimum(median, past_gain) * cdown[state] */
    test (); 
    if (sub (tmp, st->past_gain_code) > 0)
    {
        tmp = st->past_gain_code;                              move16 (); 
    }
    tmp = mult (tmp, cdown[state]);
    *gain_code = tmp;                                          move16 (); 

    /* update table of past quantized energies with average of
     * current values
     */
    gc_pred_average_limited(pred_state, &qua_ener_MR122, &qua_ener);
    gc_pred_update(pred_state, qua_ener_MR122, qua_ener);
}

/*
**************************************************************************
*
*  Function    : ec_gain_code_update
*  Purpose     : update the codebook gain concealment state;
*                limit gain_code if the previous frame was bad
*                Call this function always after decoding (or concealing)
*                the gain
*
**************************************************************************
*/
void ec_gain_code_update (
    ec_gain_codeState *st,    /* i/o : State struct                     */
    Word16 bfi,               /* i   : flag: frame is bad               */
    Word16 prev_bf,           /* i   : flag: previous frame was bad     */
    Word16 *gain_code         /* i/o : decoded innovation gain          */
)
{
    Word16 i;
    
    /* limit gain_code by previous good gain if previous frame was bad */
    test ();
    if (bfi == 0)
    {
		test ();
        if (prev_bf != 0)
        {
            test (); 
            if (sub (*gain_code, st->prev_gc) > 0)
            {
                *gain_code = st->prev_gc;     move16 (); 
            }
        }
        st->prev_gc = *gain_code;                          move16 (); 
    }

    /* update EC states: previous gain, gain buffer */
    st->past_gain_code = *gain_code;                       move16 (); 
    
    for (i = 1; i < 5; i++)
    {
        st->gbuf[i - 1] = st->gbuf[i];                     move16 (); 
    }
    st->gbuf[4] = *gain_code;                              move16 (); 

    return;
}


/*
**************************************************************************
*
*  Function    : ec_gain_pitch_init
*  Purpose     : Allocates memory and initializes state memory.
*
**************************************************************************
*/
int ec_gain_pitch_init (ec_gain_pitchState **state)
{
  ec_gain_pitchState* s;
 
  if (state == (ec_gain_pitchState **) NULL){
      fprintf(stderr, "ec_gain_pitch_init: invalid parameter\n");
      return -1;
  }
  *state = NULL;
 
  /* allocate memory */
  if ((s= (ec_gain_pitchState *) malloc(sizeof(ec_gain_pitchState))) == NULL){
      fprintf(stderr, "ec_gain_pitch_init: can not malloc state structure\n");
      return -1;
  }
  
  ec_gain_pitch_reset(s);
  *state = s;
  
  return 0;
}
 
/*
**************************************************************************
*
*  Function:   ec_gain_pitch_reset
*  Purpose:    Resets state memory
*
**************************************************************************
*/
int ec_gain_pitch_reset (ec_gain_pitchState *state)
{
  Word16 i;
  
  if (state == (ec_gain_pitchState *) NULL){
      fprintf(stderr, "ec_gain_pitch_reset: invalid parameter\n");
      return -1;
  }
  
  for(i = 0; i < 5; i++)
      state->pbuf[i] = 1640;
  state->past_gain_pit = 0; 
  state->prev_gp = 16384;   

  return 0;
}
 
/*************************************************************************
*
*  Function    : ec_gain_pitch_exit
*  Purpose     : The memory used for state memory is freed
*
**************************************************************************
*/
void ec_gain_pitch_exit (ec_gain_pitchState **state)
{
  if (state == NULL || *state == NULL)
      return;
 
  /* deallocate memory */
  free(*state);
  *state = NULL;
  
  return;
}

/*
**************************************************************************
*
*  Function    : ec_gain_pitch
*  Purpose     : conceal the pitch gain
*                Call this function only in BFI (instead of normal gain
*                decoding function)
*
**************************************************************************
*/
void ec_gain_pitch (
    ec_gain_pitchState *st, /* i/o : state variables                   */
    Word16 state,           /* i   : state of the state machine        */
    Word16 *gain_pitch      /* o   : pitch gain (Q14)                  */
)
{
    static const Word16 pdown[7] =
    {
        32767, 32112, 32112, 26214,
        9830, 6553, 6553
    };

    Word16 tmp;

    /* calculate median of last five gains */
    tmp = gmed_n (st->pbuf, 5);                        move16 (); 

    /* new gain = minimum(median, past_gain) * pdown[state] */
    test (); 
    if (sub (tmp, st->past_gain_pit) > 0)
    {
        tmp = st->past_gain_pit;                       move16 (); 
    }
    *gain_pitch = mult (tmp, pdown[state]);
}

/*
**************************************************************************
*
*  Function    : ec_gain_pitch_update
*  Purpose     : update the pitch gain concealment state;
*                limit gain_pitch if the previous frame was bad
*                Call this function always after decoding (or concealing)
*                the gain
*
**************************************************************************
*/
void ec_gain_pitch_update (
    ec_gain_pitchState *st, /* i/o : state variables                   */
    Word16 bfi,             /* i   : flag: frame is bad                */
    Word16 prev_bf,         /* i   : flag: previous frame was bad      */
    Word16 *gain_pitch      /* i/o : pitch gain                        */
)
{
    Word16 i;

    test (); 
    if (bfi == 0)
    {
        test ();
        if (prev_bf != 0)
        {
            test (); 
            if (sub (*gain_pitch, st->prev_gp) > 0)
            {
                *gain_pitch = st->prev_gp;
            }
        }
        st->prev_gp = *gain_pitch;                         move16 (); 
    }
    
    st->past_gain_pit = *gain_pitch;                       move16 ();

    test (); 
    if (sub (st->past_gain_pit, 16384) > 0)  /* if (st->past_gain_pit > 1.0) */
    {
        st->past_gain_pit = 16384;                         move16 (); 
    }
    for (i = 1; i < 5; i++)
    {
        st->pbuf[i - 1] = st->pbuf[i];                     move16 (); 
    }
    st->pbuf[4] = st->past_gain_pit;                       move16 (); 
}
