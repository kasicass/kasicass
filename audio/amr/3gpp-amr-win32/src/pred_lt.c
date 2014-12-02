/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : pred_lt.c
*      Purpose          : Compute the result of long term prediction
*
********************************************************************************
*/
 
 
/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "pred_lt.h"
const char pred_lt_id[] = "@(#)$Id $" pred_lt_h;
 
/*
********************************************************************************
*                         INCLUDE FILES
********************************************************************************
*/
#include "typedef.h"
#include "basic_op.h"
#include "count.h"
#include "cnst.h"

/*
********************************************************************************
*                         LOCAL VARIABLES AND TABLES
********************************************************************************
*/
#define UP_SAMP_MAX  6
#define L_INTER10    (L_INTERPOL-1)
#define FIR_SIZE     (UP_SAMP_MAX*L_INTER10+1)

/* 1/6 resolution interpolation filter  (-3 dB at 3600 Hz) */
/* Note: the 1/3 resolution filter is simply a subsampled
 *       version of the 1/6 resolution filter, i.e. it uses
 *       every second coefficient:
 *       
 *          inter_3l[k] = inter_6[2*k], 0 <= k <= 3*L_INTER10
 */
static const Word16 inter_6[FIR_SIZE] =
{
    29443,
    28346, 25207, 20449, 14701, 8693, 3143,
    -1352, -4402, -5865, -5850, -4673, -2783,
    -672, 1211, 2536, 3130, 2991, 2259,
    1170, 0, -1001, -1652, -1868, -1666,
    -1147, -464, 218, 756, 1060, 1099,
    904, 550, 135, -245, -514, -634,
    -602, -451, -231, 0, 191, 308,
    340, 296, 198, 78, -36, -120,
    -163, -165, -132, -79, -19, 34,
    73, 91, 89, 70, 38, 0
};

/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
/*************************************************************************
 *
 *  FUNCTION:   Pred_lt_3or6()
 *
 *  PURPOSE:  Compute the result of long term prediction with fractional
 *            interpolation of resolution 1/3 or 1/6. (Interpolated past
 *            excitation).
 *
 *  DESCRIPTION:
 *       The past excitation signal at the given delay is interpolated at
 *       the given fraction to build the adaptive codebook excitation.
 *       On return exc[0..L_subfr-1] contains the interpolated signal
 *       (adaptive codebook excitation).
 *
 *************************************************************************/
void Pred_lt_3or6 (
    Word16 exc[],     /* in/out: excitation buffer                         */
    Word16 T0,        /* input : integer pitch lag                         */
    Word16 frac,      /* input : fraction of lag                           */
    Word16 L_subfr,   /* input : subframe size                             */
    Word16 flag3      /* input : if set, upsampling rate = 3 (6 otherwise) */
)
{
    Word16 i, j, k;
    Word16 *x0, *x1, *x2;
    const Word16 *c1, *c2;
    Word32 s;

    x0 = &exc[-T0];             move16 (); 

    frac = negate (frac);
    test();
    if (flag3 != 0)
    {
      frac = shl (frac, 1);   /* inter_3l[k] = inter_6[2*k] -> k' = 2*k */
    }
    
    test (); 
    if (frac < 0)
    {
        frac = add (frac, UP_SAMP_MAX);
        x0--;
    }

    for (j = 0; j < L_subfr; j++)
    {
        x1 = x0++;              move16 (); 
        x2 = x0;                move16 (); 
        c1 = &inter_6[frac];
        c2 = &inter_6[sub (UP_SAMP_MAX, frac)];

        s = 0;                  move32 (); 
        for (i = 0, k = 0; i < L_INTER10; i++, k += UP_SAMP_MAX)
        {
            s = L_mac (s, x1[-i], c1[k]);
            s = L_mac (s, x2[i], c2[k]);
        }

        exc[j] = round (s);     move16 (); 
    }

    return;
}
