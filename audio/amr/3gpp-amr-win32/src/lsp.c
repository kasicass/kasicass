/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : lsp.c
*      Purpose          : From A(z) to lsp. LSP quantization and interpolation
*
********************************************************************************
*/

/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "lsp.h"
const char lsp_id[] = "@(#)$Id $" lsp_h;
 
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
#include "q_plsf.h"
#include "copy.h"
#include "az_lsp.h"
#include "int_lpc.h"
#include "count.h"

#include "lsp.tab"

/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
/*
**************************************************************************
*
*  Function    : lsp_init
*
**************************************************************************
*/
int lsp_init (lspState **st)
{
  lspState* s;

  if (st == (lspState **) NULL){
      fprintf(stderr, "lsp_init: invalid parameter\n");
      return -1;
  }
  
  *st = NULL;
 
  /* allocate memory */
  if ((s= (lspState *) malloc(sizeof(lspState))) == NULL){
      fprintf(stderr, "lsp_init: can not malloc state structure\n");
      return -1;
  }

  /* Initialize quantization state */
   Q_plsf_init(&s->qSt);

  lsp_reset(s);
  *st = s;
  
  return 0;
}
 
/*
**************************************************************************
*
*  Function    : lsp_reset
*
**************************************************************************
*/
int lsp_reset (lspState *st)
{
  
  if (st == (lspState *) NULL){
      fprintf(stderr, "lsp_reset: invalid parameter\n");
      return -1;
  }
  
  /* Init lsp_old[] */
  Copy(lsp_init_data, &st->lsp_old[0], M);

  /* Initialize lsp_old_q[] */
  Copy(st->lsp_old, st->lsp_old_q, M);
  
  /* Reset quantization state */
   Q_plsf_reset(st->qSt);

  return 0;
}
 
/*
**************************************************************************
*
*  Function    : lsp_exit
*
**************************************************************************
*/
void lsp_exit (lspState **st)
{
  if (st == NULL || *st == NULL)
      return;

  /* Deallocate members */
  Q_plsf_exit(&(*st)->qSt);

  /* deallocate memory */
  free(*st);
  *st = NULL;
  
  return;
}

/*************************************************************************
 *
 *   FUNCTION:  lsp()
 *
 ************************************************************************/
int lsp(lspState *st,        /* i/o : State struct                            */
        enum Mode req_mode,  /* i   : requested coder mode                    */
        enum Mode used_mode, /* i   : used coder mode                         */        
        Word16 az[],         /* i/o : interpolated LP parameters Q12          */
        Word16 azQ[],        /* o   : quantization interpol. LP parameters Q12*/
        Word16 lsp_new[],    /* o   : new lsp vector                          */ 
        Word16 **anap        /* o   : analysis parameters                     */)
{
   Word16 lsp_new_q[M];    /* LSPs at 4th subframe           */
   Word16 lsp_mid[M], lsp_mid_q[M];    /* LSPs at 2nd subframe           */
  
   Word16 pred_init_i; /* init index for MA prediction in DTX mode */

   test ();
   if ( sub (req_mode, MR122) == 0)
   {
       Az_lsp (&az[MP1], lsp_mid, st->lsp_old);
       Az_lsp (&az[MP1 * 3], lsp_new, lsp_mid);

       /*--------------------------------------------------------------------*
        * Find interpolated LPC parameters in all subframes (both quantized  *
        * and unquantized).                                                  *
        * The interpolated parameters are in array A_t[] of size (M+1)*4     *
        * and the quantized interpolated parameters are in array Aq_t[]      *
        *--------------------------------------------------------------------*/
       Int_lpc_1and3_2 (st->lsp_old, lsp_mid, lsp_new, az);

       test ();
       if ( sub (used_mode, MRDTX) != 0)
       {
          /* LSP quantization (lsp_mid[] and lsp_new[] jointly quantized) */
          Q_plsf_5 (st->qSt, lsp_mid, lsp_new, lsp_mid_q, lsp_new_q, *anap);
       
          Int_lpc_1and3 (st->lsp_old_q, lsp_mid_q, lsp_new_q, azQ);
          
          /* Advance analysis parameters pointer */
          (*anap) += add(0,5); move16 ();
       }	 
   }
   else
   {
       Az_lsp(&az[MP1 * 3], lsp_new, st->lsp_old);  /* From A(z) to lsp  */
       
       /*--------------------------------------------------------------------*
        * Find interpolated LPC parameters in all subframes (both quantized  *
        * and unquantized).                                                  *
        * The interpolated parameters are in array A_t[] of size (M+1)*4     *
        * and the quantized interpolated parameters are in array Aq_t[]      *
        *--------------------------------------------------------------------*/
       
       Int_lpc_1to3_2(st->lsp_old, lsp_new, az);
       
       test ();
       if ( sub (used_mode, MRDTX) != 0)
       {
          /* LSP quantization */
          Q_plsf_3(st->qSt, req_mode, lsp_new, lsp_new_q, *anap, &pred_init_i);
          
          Int_lpc_1to3(st->lsp_old_q, lsp_new_q, azQ);
          
          /* Advance analysis parameters pointer */
          (*anap) += add (0, 3); move16 ();
       }
   }
       
   /* update the LSPs for the next frame */   
   Copy (lsp_new, st->lsp_old, M);
   Copy (lsp_new_q, st->lsp_old_q, M);

   return 0;
}

