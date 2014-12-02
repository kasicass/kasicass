/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : ol_ltp.c
*      Purpose          : Compute the open loop pitch lag.
*
********************************************************************************
*/

/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "ol_ltp.h"
const char ol_ltp_id[] = "@(#)$Id $" ol_ltp_h;
 
/*
********************************************************************************
*                         INCLUDE FILES
********************************************************************************
*/
#include <stdio.h>
#include "typedef.h"
#include "cnst.h"
#include "pitch_ol.h"
#include "p_ol_wgh.h"
#include "count.h"
#include "basic_op.h"

/*
********************************************************************************
*                         LOCAL VARIABLES AND TABLES
********************************************************************************
*/

/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
int ol_ltp(
    pitchOLWghtState *st, /* i/o : State struct                            */
    vadState *vadSt,      /* i/o : VAD state struct                        */
    enum Mode mode,       /* i   : coder mode                              */
    Word16 wsp[],         /* i   : signal used to compute the OL pitch, Q0 */
                          /*       uses signal[-pit_max] to signal[-1]     */
    Word16 *T_op,         /* o   : open loop pitch lag,                 Q0 */
    Word16 old_lags[],    /* i   : history with old stored Cl lags         */
    Word16 ol_gain_flg[], /* i   : OL gain flag                            */
    Word16 idx,           /* i   : index                                   */
    Flag dtx              /* i   : dtx flag; use dtx=1, do not use dtx=0   */
    )
{
   test ();   
   if (sub (mode, MR102) != 0 )
   {
      ol_gain_flg[0] = 0;                                       move16 ();
      ol_gain_flg[1] = 0;                                       move16 ();
   }
   
   test (); test ();
   if (sub (mode, MR475) == 0 || sub (mode, MR515) == 0 )
   {
      *T_op = Pitch_ol(vadSt, mode, wsp, PIT_MIN, PIT_MAX, L_FRAME, idx, dtx);
                                                                move16 ();
   }
   else
   {
      if ( sub (mode, MR795) <= 0 )
      {
         test();
         *T_op = Pitch_ol(vadSt, mode, wsp, PIT_MIN, PIT_MAX, L_FRAME_BY2,
                          idx, dtx);
                                                                move16 ();
      }
      else if ( sub (mode, MR102) == 0 )
      {
         test(); test();
         *T_op = Pitch_ol_wgh(st, vadSt, wsp, PIT_MIN, PIT_MAX, L_FRAME_BY2,
                              old_lags, ol_gain_flg, idx, dtx);
                                                                move16 ();
      }
      else
      {
         test(); test();          
         *T_op = Pitch_ol(vadSt, mode, wsp, PIT_MIN_MR122, PIT_MAX,
                          L_FRAME_BY2, idx, dtx);
                                                                move16 ();
      }
   }
   return 0;
}
