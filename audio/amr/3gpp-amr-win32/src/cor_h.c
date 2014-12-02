/*
*****************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
******************************************************************************
*
*      File             : cor_h.c
*      Purpose          : correlation functions for codebook search
*
*****************************************************************************
*/
/*
*****************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
*****************************************************************************
*/
#include "cor_h.h"
const char cor_h_id[] = "@(#)$Id $" cor_h_h;
/*
*****************************************************************************
*                         INCLUDE FILES
*****************************************************************************
*/
#include "typedef.h"
#include "basic_op.h"
#include "count.h"
#include "inv_sqrt.h"
#include "cnst.h" 
 
/*
*****************************************************************************
*                         PUBLIC PROGRAM CODE
*****************************************************************************
*/
/*************************************************************************
 *
 *  FUNCTION:  cor_h_x()
 *
 *  PURPOSE:  Computes correlation between target signal "x[]" and
 *            impulse response"h[]".
 *
 *  DESCRIPTION:
 *    The correlation is given by:
 *       d[n] = sum_{i=n}^{L-1} x[i] h[i-n]      n=0,...,L-1
 *
 *    d[n] is normalized such that the sum of 5 maxima of d[n] corresponding
 *    to each position track does not saturate.
 *
 *************************************************************************/
void cor_h_x (
    Word16 h[],    /* (i): impulse response of weighted synthesis filter */
    Word16 x[],    /* (i): target                                        */
    Word16 dn[],   /* (o): correlation between target and h[]            */
    Word16 sf      /* (i): scaling factor: 2 for 12.2, 1 for others      */
)
{
    cor_h_x2(h, x, dn, sf, NB_TRACK, STEP);
}

/*************************************************************************
 *
 *  FUNCTION:  cor_h_x2()
 *
 *  PURPOSE:  Computes correlation between target signal "x[]" and
 *            impulse response"h[]".
 *
 *  DESCRIPTION:
 *            See cor_h_x, d[n] can be normalized regards to sum of the
 *            five MR122 maxima or the four MR102 maxima.
 *
 *************************************************************************/
void cor_h_x2 (
    Word16 h[],    /* (i): impulse response of weighted synthesis filter */
    Word16 x[],    /* (i): target                                        */
    Word16 dn[],   /* (o): correlation between target and h[]            */
    Word16 sf,     /* (i): scaling factor: 2 for 12.2, 1 for others      */
    Word16 nb_track,/* (i): the number of ACB tracks                     */
    Word16 step    /* (i): step size from one pulse position to the next
                           in one track                                  */
)
{
    Word16 i, j, k;
    Word32 s, y32[L_CODE], max, tot;

    /* first keep the result on 32 bits and find absolute maximum */

    tot = 5;                                     move32 (); 

    for (k = 0; k < nb_track; k++)
    {
        max = 0;                                 move32 (); 
        for (i = k; i < L_CODE; i += step)
        {
            s = 0;                               move32 (); 
            for (j = i; j < L_CODE; j++)
                s = L_mac (s, x[j], h[j - i]);
            
            y32[i] = s;                          move32 (); 
            
            s = L_abs (s);
            test (); 
            if (L_sub (s, max) > (Word32) 0L)
                max = s;                         move32 (); 
        }
        tot = L_add (tot, L_shr (max, 1));
    }
    
    j = sub (norm_l (tot), sf);
    
    for (i = 0; i < L_CODE; i++)
    {
        dn[i] = round (L_shl (y32[i], j));       move16 (); 
    }
}

/*************************************************************************
 *
 *  FUNCTION:  cor_h()
 *
 *  PURPOSE:  Computes correlations of h[] needed for the codebook search;
 *            and includes the sign information into the correlations.
 *
 *  DESCRIPTION: The correlations are given by
 *         rr[i][j] = sum_{n=i}^{L-1} h[n-i] h[n-j];   i>=j; i,j=0,...,L-1
 *
 *  and the sign information is included by
 *         rr[i][j] = rr[i][j]*sign[i]*sign[j]
 *
 *************************************************************************/

void cor_h (
    Word16 h[],         /* (i) : impulse response of weighted synthesis
                                 filter                                  */
    Word16 sign[],      /* (i) : sign of d[n]                            */
    Word16 rr[][L_CODE] /* (o) : matrix of autocorrelation               */
)
{
    Word16 i, j, k, dec, h2[L_CODE];
    Word32 s;

    /* Scaling for maximum precision */

    s = 2;                                       move32 (); 
    for (i = 0; i < L_CODE; i++)
        s = L_mac (s, h[i], h[i]);
    
    j = sub (extract_h (s), 32767);
    test (); 
    if (j == 0)
    {
        for (i = 0; i < L_CODE; i++)
        {
            h2[i] = shr (h[i], 1);               move16 (); 
        }
    }
    else
    {
        s = L_shr (s, 1);
        k = extract_h (L_shl (Inv_sqrt (s), 7));
        k = mult (k, 32440);                     /* k = 0.99*k */
        
        for (i = 0; i < L_CODE; i++)
        {
            h2[i] = round (L_shl (L_mult (h[i], k), 9));
                                                 move16 (); 
        }
    }
    
    /* build matrix rr[] */
    s = 0;                                       move32 (); 
    i = L_CODE - 1;
    for (k = 0; k < L_CODE; k++, i--)
    {
        s = L_mac (s, h2[k], h2[k]);
        rr[i][i] = round (s);                    move16 (); 
    }
    
    for (dec = 1; dec < L_CODE; dec++)
    {
        s = 0;                                   move32 (); 
        j = L_CODE - 1;
        i = sub (j, dec);
        for (k = 0; k < (L_CODE - dec); k++, i--, j--)
        {
            s = L_mac (s, h2[k], h2[k + dec]);
            rr[j][i] = mult (round (s), mult (sign[i], sign[j]));
                                                 move16 (); 
            rr[i][j] = rr[j][i];                 move16 (); 
        }
    }
}
