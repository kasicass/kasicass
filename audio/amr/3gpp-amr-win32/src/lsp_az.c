/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : lsp_az.c
*      Purpose          : Converts from the line spectral pairs (LSP) to
*                       : LP coefficients, for a 10th order filter.
*      Description      : 
*                 - Find the coefficients of F1(z) and F2(z) (see Get_lsp_pol)
*                 - Multiply F1(z) by 1+z^{-1} and F2(z) by 1-z^{-1}
*                 - A(z) = ( F1(z) + F2(z) ) / 2
*
********************************************************************************
*/
/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "lsp_az.h"
const char lsp_az_id[] = "@(#)$Id $" 
lsp_az_h;
/*
********************************************************************************
*                         INCLUDE FILES
********************************************************************************
*/
#include "typedef.h"
#include "basic_op.h"
#include "oper_32b.h"
#include "count.h"

/*
********************************************************************************
*                         LOCAL VARIABLES AND TABLES
********************************************************************************
*/

/*
********************************************************************************
*                         LOCAL PROGRAM CODE
********************************************************************************
*/
/*************************************************************************
 *
 *  FUNCTION:  Get_lsp_pol
 *
 *  PURPOSE:  Find the polynomial F1(z) or F2(z) from the LSPs.
 *            If the LSP vector is passed at address 0  F1(z) is computed
 *            and if it is passed at address 1  F2(z) is computed.
 *
 *  DESCRIPTION:
 *       This is performed by expanding the product polynomials:
 *
 *           F1(z) =   product   ( 1 - 2 lsp[i] z^-1 + z^-2 )
 *                   i=0,2,4,6,8
 *           F2(z) =   product   ( 1 - 2 lsp[i] z^-1 + z^-2 )
 *                   i=1,3,5,7,9
 *
 *       where lsp[] is the LSP vector in the cosine domain.
 *
 *       The expansion is performed using the following recursion:
 *
 *            f[0] = 1
 *            b = -2.0 * lsp[0]
 *            f[1] = b
 *            for i=2 to 5 do
 *               b = -2.0 * lsp[2*i-2];
 *               f[i] = b*f[i-1] + 2.0*f[i-2];
 *               for j=i-1 down to 2 do
 *                   f[j] = f[j] + b*f[j-1] + f[j-2];
 *               f[1] = f[1] + b;
 *
 *************************************************************************/

static void Get_lsp_pol (Word16 *lsp, Word32 *f)
{
    Word16 i, j, hi, lo;
    Word32 t0;
    
    /* f[0] = 1.0;             */
    *f = L_mult (4096, 2048);              move32 (); 
    f++;                                   move32 (); 
    *f = L_msu ((Word32) 0, *lsp, 512);    /* f[1] =  -2.0 * lsp[0];  */
    f++;                                   move32 (); 
    lsp += 2;                              /* Advance lsp pointer     */

    for (i = 2; i <= 5; i++)
    {
        *f = f[-2];                        move32 (); 

        for (j = 1; j < i; j++, f--)
        {
            L_Extract (f[-1], &hi, &lo);
            t0 = Mpy_32_16 (hi, lo, *lsp); /* t0 = f[-1] * lsp    */
            t0 = L_shl (t0, 1);
            *f = L_add (*f, f[-2]);        move32 (); /* *f += f[-2]      */
            *f = L_sub (*f, t0);move32 (); /* *f -= t0            */
        }
        *f = L_msu (*f, *lsp, 512);        move32 (); /* *f -= lsp<<9     */
        f += i;                            /* Advance f pointer   */
        lsp += 2;                          /* Advance lsp pointer */
    }

    return;
}

/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
/*************************************************************************
 *
 *  FUNCTION:   Lsp_Az
 *
 *  PURPOSE:  Converts from the line spectral pairs (LSP) to
 *            LP coefficients, for a 10th order filter.
 *
 *  DESCRIPTION:
 *     - Find the coefficients of F1(z) and F2(z) (see Get_lsp_pol)
 *     - Multiply F1(z) by 1+z^{-1} and F2(z) by 1-z^{-1}
 *     - A(z) = ( F1(z) + F2(z) ) / 2
 *
 *************************************************************************/
void Lsp_Az (
    Word16 lsp[],        /* (i)  : line spectral frequencies            */
    Word16 a[]           /* (o)  : predictor coefficients (order = 10)  */
)
{
    Word16 i, j;
    Word32 f1[6], f2[6];
    Word32 t0;

    Get_lsp_pol (&lsp[0], f1);
    Get_lsp_pol (&lsp[1], f2);

    for (i = 5; i > 0; i--)
    {
        f1[i] = L_add (f1[i], f1[i - 1]);    move32 (); /* f1[i] += f1[i-1]; */
        f2[i] = L_sub (f2[i], f2[i - 1]);    move32 (); /* f2[i] -= f2[i-1]; */
    }

    a[0] = 4096;                             move16 (); 
    for (i = 1, j = 10; i <= 5; i++, j--)
    {
        t0 = L_add (f1[i], f2[i]);           /* f1[i] + f2[i] */
        a[i] = extract_l (L_shr_r (t0, 13)); move16 (); 
        t0 = L_sub (f1[i], f2[i]);           /* f1[i] - f2[i] */
        a[j] = extract_l (L_shr_r (t0, 13)); move16 (); 
    }

    return;
}

