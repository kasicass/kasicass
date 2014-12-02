/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : calc_cor.c
*      Purpose          : Calculate all correlations for prior the OL LTP 
*
********************************************************************************
*/

/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "calc_cor.h"
const char calc_cor_id[] = "@(#)$Id $" calc_cor_h;

/*
********************************************************************************
*                         INCLUDE FILES
********************************************************************************
*/
#include <stdio.h>
#include <stdlib.h>
#include "typedef.h"
#include "basic_op.h"
#include "oper_32b.h"
#include "count.h"
#include "cnst.h"

/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
/*************************************************************************
 *
 *  FUNCTION: comp_corr 
 *
 *  PURPOSE: Calculate all correlations of scal_sig[] in a given delay
 *           range.
 *
 *  DESCRIPTION:
 *      The correlation is given by
 *           cor[t] = <scal_sig[n],scal_sig[n-t]>,  t=lag_min,...,lag_max
 *      The functions outputs the all correlations 
 *
 *************************************************************************/
void comp_corr ( 
    Word16 scal_sig[],  /* i   : scaled signal.                          */
    Word16 L_frame,     /* i   : length of frame to compute pitch        */
    Word16 lag_max,     /* i   : maximum lag                             */
    Word16 lag_min,     /* i   : minimum lag                             */
    Word32 corr[])      /* o   : correlation of selected lag             */
{
    Word16 i, j;
    Word16 *p, *p1;
    Word32 t0;
    
    for (i = lag_max; i >= lag_min; i--)
    {
       p = scal_sig;           move16 (); 
       p1 = &scal_sig[-i];     move16 (); 
       t0 = 0;                 move32 (); 
       
       for (j = 0; j < L_frame; j++, p++, p1++)
       {
          t0 = L_mac (t0, *p, *p1);             
       }
       corr[-i] = t0;          move32 ();
    }

    return;
}
