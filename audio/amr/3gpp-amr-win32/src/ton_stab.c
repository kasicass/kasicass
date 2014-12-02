/*
*****************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
*****************************************************************************
*
*      File             : ton_stab.c
*
*****************************************************************************
*/

/*
*****************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
*****************************************************************************
*/
#include "ton_stab.h"
const char ton_stab_id[] = "@(#)$Id $" ton_stab_h;
 
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
#include "set_zero.h"
#include "copy.h"

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
 *  Function:   ton_stab_init
 *  Purpose:    Allocates state memory and initializes state memory
 *
 **************************************************************************
 */
int ton_stab_init (tonStabState **state)
{
    tonStabState* s;
    
    if (state == (tonStabState **) NULL){
        fprintf(stderr, "ton_stab_init: invalid parameter\n");
        return -1;
    }
    *state = NULL;
    
    /* allocate memory */
    if ((s= (tonStabState *) malloc(sizeof(tonStabState))) == NULL){
        fprintf(stderr, "ton_stab_init: can not malloc state structure\n");
        return -1;
    }
    
    ton_stab_reset(s);
    
    *state = s;
    
    return 0;
}

/*************************************************************************
 *
 *  Function:   ton_stab_reset
 *  Purpose:    Initializes state memory to zero
 *
 **************************************************************************
 */
int ton_stab_reset (tonStabState *st)
{
    if (st == (tonStabState *) NULL){
        fprintf(stderr, "ton_stab_init: invalid parameter\n");
        return -1;
    }

    /* initialize tone stabilizer state */ 
    st->count = 0;
    Set_zero(st->gp, N_FRAME);    /* Init Gp_Clipping */
    
    return 0;
}

/*************************************************************************
 *
 *  Function:   ton_stab_exit
 *  Purpose:    The memory used for state memory is freed
 *
 **************************************************************************
 */
void ton_stab_exit (tonStabState **state)
{
    if (state == NULL || *state == NULL)
        return;

    /* deallocate memory */
    free(*state);
    *state = NULL;
    
    return;
}

/***************************************************************************
 *                                                                          *
 *  Function:  check_lsp()                                                  *
 *  Purpose:   Check the LSP's to detect resonances                         *
 *                                                                          *
 ****************************************************************************
 */
Word16 check_lsp(tonStabState *st, /* i/o : State struct            */
                 Word16 *lsp       /* i   : unquantized LSP's       */
)
{
   Word16 i, dist, dist_min1, dist_min2, dist_th;
 
   /* Check for a resonance:                             */
   /* Find minimum distance between lsp[i] and lsp[i+1]  */
 
   dist_min1 = MAX_16;                       move16 ();
   for (i = 3; i < M-2; i++)
   {
      dist = sub(lsp[i], lsp[i+1]);

      test ();
      if (sub(dist, dist_min1) < 0)
      {
         dist_min1 = dist;                   move16 ();
      }
   }

   dist_min2 = MAX_16;                       move16 ();
   for (i = 1; i < 3; i++)
   {
      dist = sub(lsp[i], lsp[i+1]);

      test ();
      if (sub(dist, dist_min2) < 0)
      {
         dist_min2 = dist;                   move16 ();
      }
   }

   if (test (), sub(lsp[1], 32000) > 0)
   {
      dist_th = 600;                         move16 ();
   }
   else if (test (), sub(lsp[1], 30500) > 0)
   {
      dist_th = 800;                         move16 ();
   }
   else
   {
      dist_th = 1100;                        move16 ();
   }

   test (); test ();
   if (sub(dist_min1, 1500) < 0 ||
       sub(dist_min2, dist_th) < 0)
   {
      st->count = add(st->count, 1);
   }
   else
   {
      st->count = 0;                         move16 ();
   }
   
   /* Need 12 consecutive frames to set the flag */
   test ();
   if (sub(st->count, 12) >= 0)
   {
      st->count = 12;                        move16 ();
      return 1;
   }
   else
   {
      return 0;
   }
}

/***************************************************************************
 *
 *  Function:   Check_Gp_Clipping()                                          
 *  Purpose:    Verify that the sum of the last (N_FRAME+1) pitch  
 *              gains is under a certain threshold.              
 *                                                                         
 ***************************************************************************
 */ 
Word16 check_gp_clipping(tonStabState *st, /* i/o : State struct            */
                         Word16 g_pitch    /* i   : pitch gain              */
)
{
   Word16 i, sum;
   
   sum = shr(g_pitch, 3);          /* Division by 8 */
   for (i = 0; i < N_FRAME; i++)
   {
      sum = add(sum, st->gp[i]);
   }

   test ();
   if (sub(sum, GP_CLIP) > 0)
   {
      return 1;
   }
   else
   {
      return 0;
   }
}

/***************************************************************************
 *
 *  Function:  Update_Gp_Clipping()                                          
 *  Purpose:   Update past pitch gain memory
 *                                                                         
 ***************************************************************************
 */
void update_gp_clipping(tonStabState *st, /* i/o : State struct            */
                        Word16 g_pitch    /* i   : pitch gain              */
)
{
   Copy(&st->gp[1], &st->gp[0], N_FRAME-1);
   st->gp[N_FRAME-1] = shr(g_pitch, 3);
}
