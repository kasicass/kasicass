/*************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : bgnscd.c
*      Purpose          : Background noise source charateristic detector (SCD)
*
********************************************************************************
*/

/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "bgnscd.h"
const char bgnscd_id[] = "@(#)$Id $" bgnscd_h;

#include <stdlib.h>
#include <stdio.h>
#include "typedef.h"
#include "basic_op.h"
#include "count.h"
#include "cnst.h"
#include "copy.h"
#include "set_zero.h"
#include "gmed_n.h"
#include "sqrt_l.h"

/*
********************************************************************************
*                         LOCAL VARIABLES AND TABLES
********************************************************************************
*/
/*-----------------------------------------------------------------*
 *   Decoder constant parameters (defined in "cnst.h")             *
 *-----------------------------------------------------------------*
 *   L_FRAME       : Frame size.                                   *
 *   L_SUBFR       : Sub-frame size.                               *
 *-----------------------------------------------------------------*/

/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
/*
**************************************************************************
*
*  Function    : Bgn_scd_init
*  Purpose     : Allocates and initializes state memory
*
**************************************************************************
*/
Word16 Bgn_scd_init (Bgn_scdState **state)
{
   Bgn_scdState* s;
   
   if (state == (Bgn_scdState **) NULL){
      fprintf(stderr, "Bgn_scd_init: invalid parameter\n");
      return -1;
   }
   *state = NULL;
   
   /* allocate memory */
   if ((s= (Bgn_scdState *) malloc(sizeof(Bgn_scdState))) == NULL){
     fprintf(stderr, "Bgn_scd_init: can not malloc state structure\n");
     return -1;
   }
   
   Bgn_scd_reset(s);
   *state = s;
   
   return 0;
}

/*
**************************************************************************
*
*  Function    : Bgn_scd_reset
*  Purpose     : Resets state memory
*
**************************************************************************
*/
Word16 Bgn_scd_reset (Bgn_scdState *state)
{
   if (state == (Bgn_scdState *) NULL){
      fprintf(stderr, "Bgn_scd_reset: invalid parameter\n");
      return -1;
   }

   /* Static vectors to zero */
   Set_zero (state->frameEnergyHist, L_ENERGYHIST);

   /* Initialize hangover handling */
   state->bgHangover = 0;
   
   return 0;
}

/*
**************************************************************************
*
*  Function    : Bgn_scd_exit
*  Purpose     : The memory used for state memory is freed
*
**************************************************************************
*/
void Bgn_scd_exit (Bgn_scdState **state)
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
*  Function    : Bgn_scd
*  Purpose     : Charaterice synthesis speech and detect background noise
*  Returns     : background noise decision; 0 = no bgn, 1 = bgn
*
**************************************************************************
*/
Word16 Bgn_scd (Bgn_scdState *st,      /* i : State variables for bgn SCD */
                Word16 ltpGainHist[],  /* i : LTP gain history            */
                Word16 speech[],       /* o : synthesis speech frame      */
                Word16 *voicedHangover /* o : # of frames after last 
                                              voiced frame                */
                )
{
   Word16 i;
   Word16 prevVoiced, inbgNoise;
   Word16 temp;
   Word16 ltpLimit, frameEnergyMin;
   Word16 currEnergy, noiseFloor, maxEnergy, maxEnergyLastPart;
   Word32 s;
   
   /* Update the inBackgroundNoise flag (valid for use in next frame if BFI) */
   /* it now works as a energy detector floating on top                      */ 
   /* not as good as a VAD.                                                  */

   currEnergy = 0;                                   move16 ();
   s = (Word32) 0;                                   move32 ();

   for (i = 0; i < L_FRAME; i++)
   {
       s = L_mac (s, speech[i], speech[i]);
   }

   s = L_shl(s, 2);  

   currEnergy = extract_h (s);

   frameEnergyMin = 32767;                     move16 ();

   for (i = 0; i < L_ENERGYHIST; i++)
   {
      test ();
      if (sub(st->frameEnergyHist[i], frameEnergyMin) < 0)
         frameEnergyMin = st->frameEnergyHist[i];           move16 ();
   }

   noiseFloor = shl (frameEnergyMin, 4); /* Frame Energy Margin of 16 */

   maxEnergy = st->frameEnergyHist[0];               move16 ();
   for (i = 1; i < L_ENERGYHIST-4; i++)
   {
      test ();
      if ( sub (maxEnergy, st->frameEnergyHist[i]) < 0)
      {
         maxEnergy = st->frameEnergyHist[i];         move16 ();
      }
   }
   
   maxEnergyLastPart = st->frameEnergyHist[2*L_ENERGYHIST/3]; move16 ();
   for (i = 2*L_ENERGYHIST/3+1; i < L_ENERGYHIST; i++)
   {
      test ();
      if ( sub (maxEnergyLastPart, st->frameEnergyHist[i] ) < 0)
      {
         maxEnergyLastPart = st->frameEnergyHist[i]; move16 ();     
      }
   }

   inbgNoise = 0;        /* false */                 move16 (); 

   /* Do not consider silence as noise */
   /* Do not consider continuous high volume as noise */
   /* Or if the current noise level is very low */
   /* Mark as noise if under current noise limit */
   /* OR if the maximum energy is below the upper limit */

   test (); test (); test (); test (); test (); 
   if ( (sub(maxEnergy, LOWERNOISELIMIT) > 0) &&
        (sub(currEnergy, FRAMEENERGYLIMIT) < 0) &&
        (sub(currEnergy, LOWERNOISELIMIT) > 0) &&
        ( (sub(currEnergy, noiseFloor) < 0) ||
          (sub(maxEnergyLastPart, UPPERNOISELIMIT) < 0)))
   {
      test ();
      if (sub(add(st->bgHangover, 1), 30) > 0)
      {
         st->bgHangover = 30;                         move16 ();
      } else
      {
         st->bgHangover = add(st->bgHangover, 1);
      }
   }
   else
   {
      st->bgHangover = 0;                             move16 ();    
   }
   
   /* make final decision about frame state , act somewhat cautiosly */
   test ();
   if (sub(st->bgHangover,1) > 0)
      inbgNoise = 1;       /* true  */               move16 ();  

   for (i = 0; i < L_ENERGYHIST-1; i++)
   {
      st->frameEnergyHist[i] = st->frameEnergyHist[i+1]; move16 ();
   }
   st->frameEnergyHist[L_ENERGYHIST-1] = currEnergy;              move16 ();
   
   /* prepare for voicing decision; tighten the threshold after some 
      time in noise */
   ltpLimit = 13926;             /* 0.85  Q14 */     move16 (); 
   test ();
   if (sub(st->bgHangover, 8) > 0)
   {
      ltpLimit = 15565;          /* 0.95  Q14 */     move16 ();
   }
   test ();
   if (sub(st->bgHangover, 15) > 0)
   {
      ltpLimit = 16383;          /* 1.00  Q14 */     move16 ();
   }

   /* weak sort of voicing indication. */
   prevVoiced = 0;        /* false */                move16 ();
   test ();

   if (sub(gmed_n(&ltpGainHist[4], 5), ltpLimit) > 0)
   {
      prevVoiced = 1;     /* true  */                move16 ();
   }
   test ();   
   if (sub(st->bgHangover, 20) > 0) {
      if (sub(gmed_n(ltpGainHist, 9), ltpLimit) > 0)
      {
         prevVoiced = 1;  /* true  */                move16 ();
      }
      else
      {
         prevVoiced = 0;  /* false  */                move16 ();
      }
   }
   
   test ();
   if (prevVoiced)
   {
      *voicedHangover = 0;                        move16 ();
   }
   else
   {
      temp = add(*voicedHangover, 1);
      test ();
      if (sub(temp, 10) > 0)
      {
         *voicedHangover = 10;                    move16 ();
      }
      else
      {
         *voicedHangover = temp;                  move16 ();
      }
   }

   return inbgNoise;
}
