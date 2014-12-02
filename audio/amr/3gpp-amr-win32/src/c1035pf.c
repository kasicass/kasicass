/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : c1035pf.c
*      Purpose          : Searches a 35 bit algebraic codebook containing 
*                       : 10 pulses in a frame of 40 samples.
*
********************************************************************************
*/
/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "c1035pf.h"
const char c1035pf_id[] = "@(#)$Id $" c1035pf_h;
/*
********************************************************************************
*                         INCLUDE FILES
********************************************************************************
*/
#include "typedef.h"
#include "basic_op.h"
#include "count.h"
#include "cnst.h"
#include "inv_sqrt.h"
#include "set_sign.h"
#include "cor_h.h"
#include "s10_8pf.h"

/*
********************************************************************************
*                         LOCAL VARIABLES AND TABLES
********************************************************************************
*/
#define NB_PULSE  10

#include "gray.tab"

/*
********************************************************************************
*                         LOCAL PROGRAM CODE
********************************************************************************
*/
void q_p (
    Word16 *ind,        /* Pulse position */
    Word16 n            /* Pulse number   */
)
{
    Word16 tmp;
    
    tmp = *ind;                                  move16 (); 
    
    test ();
    if (sub (n, 5) < 0)
    {
        *ind = (tmp & 0x8) | gray[tmp & 0x7];     logic16 (); logic16 ();
                                                 logic16 (); move16 ();
    }
    else
    {
        *ind = gray[tmp & 0x7];                   logic16 (); move16 (); 
    }
}


/*************************************************************************
 *
 *  FUNCTION:  build_code()
 *
 *  PURPOSE: Builds the codeword, the filtered codeword and index of the
 *           codevector, based on the signs and positions of 10 pulses.
 *
 *************************************************************************/

static void build_code (
    Word16 codvec[], /* (i)  : position of pulses                           */
    Word16 sign[],   /* (i)  : sign of d[n]                                 */
    Word16 cod[],    /* (o)  : innovative code vector                       */
    Word16 h[],      /* (i)  : impulse response of weighted synthesis filter*/
    Word16 y[],      /* (o)  : filtered innovative code                     */
    Word16 indx[]    /* (o)  : index of 10 pulses (sign+position)           */
)
{
    Word16 i, j, k, track, index, _sign[NB_PULSE];
    Word16 *p0, *p1, *p2, *p3, *p4, *p5, *p6, *p7, *p8, *p9;
    Word32 s;

    for (i = 0; i < L_CODE; i++)
    {
        cod[i] = 0;                              move16 (); 
    }
    for (i = 0; i < NB_TRACK; i++)
    {
        indx[i] = -1;                            move16 (); 
    }
    
    for (k = 0; k < NB_PULSE; k++)
    {
        /* read pulse position */            
        i = codvec[k];                           move16 ();
        /* read sign           */        
        j = sign[i];                             move16 (); 
        
        index = mult (i, 6554);                  /* index = pos/5       */
        /* track = pos%5 */
        track = sub (i, extract_l (L_shr (L_mult (index, 5), 1)));
        test (); 
        if (j > 0)
        {
            cod[i] = add (cod[i], 4096);         move16 ();
            _sign[k] = 8192;                     move16 (); 
            
        }
        else
        {
            cod[i] = sub (cod[i], 4096);         move16 ();
            _sign[k] = -8192;                    move16 (); 
            index = add (index, 8);
        }
        
        test (); move16 ();
        if (indx[track] < 0)
        {
            indx[track] = index;                 move16 (); 
        }
        else
        {
            test (); logic16 (); logic16 (); 
            if (((index ^ indx[track]) & 8) == 0)
            {
                /* sign of 1st pulse == sign of 2nd pulse */
                
                test (); 
                if (sub (indx[track], index) <= 0)
                {
                    indx[track + 5] = index;     move16 (); 
                }
                else
                {
                    indx[track + 5] = indx[track];
                                                 move16 (); 
                    indx[track] = index;         move16 (); 
                }
            }
            else
            {
                /* sign of 1st pulse != sign of 2nd pulse */
                
                test (); logic16 (); logic16 (); 
                if (sub ((indx[track] & 7), (index & 7)) <= 0)
                {
                    indx[track + 5] = indx[track];
                                                 move16 (); 
                    indx[track] = index;         move16 (); 
                }
                else
                {
                    indx[track + 5] = index;     move16 (); 
                }
            }
        }
    }
    
    p0 = h - codvec[0];                          move16 (); 
    p1 = h - codvec[1];                          move16 (); 
    p2 = h - codvec[2];                          move16 (); 
    p3 = h - codvec[3];                          move16 (); 
    p4 = h - codvec[4];                          move16 (); 
    p5 = h - codvec[5];                          move16 (); 
    p6 = h - codvec[6];                          move16 (); 
    p7 = h - codvec[7];                          move16 (); 
    p8 = h - codvec[8];                          move16 (); 
    p9 = h - codvec[9];                          move16 (); 
     
    for (i = 0; i < L_CODE; i++)
    {
        s = 0;                                   move32 (); 
        s = L_mac (s, *p0++, _sign[0]);
        s = L_mac (s, *p1++, _sign[1]);
        s = L_mac (s, *p2++, _sign[2]);
        s = L_mac (s, *p3++, _sign[3]);
        s = L_mac (s, *p4++, _sign[4]);
        s = L_mac (s, *p5++, _sign[5]);
        s = L_mac (s, *p6++, _sign[6]);
        s = L_mac (s, *p7++, _sign[7]);
        s = L_mac (s, *p8++, _sign[8]);
        s = L_mac (s, *p9++, _sign[9]);
        y[i] = round (s);                        move16 (); 
    }
}

/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
/*************************************************************************
 *
 *  FUNCTION:  code_10i40_35bits()
 *
 *  PURPOSE:  Searches a 35 bit algebraic codebook containing 10 pulses
 *            in a frame of 40 samples.
 *
 *  DESCRIPTION:
 *    The code contains 10 nonzero pulses: i0...i9.
 *    All pulses can have two possible amplitudes: +1 or -1.
 *    The 40 positions in a subframe are divided into 5 tracks of
 *    interleaved positions. Each track contains two pulses.
 *    The pulses can have the following possible positions:
 *
 *       i0, i5 :  0, 5, 10, 15, 20, 25, 30, 35.
 *       i1, i6 :  1, 6, 11, 16, 21, 26, 31, 36.
 *       i2, i7 :  2, 7, 12, 17, 22, 27, 32, 37.
 *       i3, i8 :  3, 8, 13, 18, 23, 28, 33, 38.
 *       i4, i9 :  4, 9, 14, 19, 24, 29, 34, 39.
 *
 *    Each pair of pulses require 1 bit for their signs and 6 bits for their
 *    positions (3 bits + 3 bits). This results in a 35 bit codebook.
 *    The function determines the optimal pulse signs and positions, builds
 *    the codevector, and computes the filtered codevector.
 *
 *************************************************************************/

void code_10i40_35bits (
    Word16 x[],   /* (i)   : target vector                                 */
    Word16 cn[],  /* (i)   : residual after long term prediction           */
    Word16 h[],   /* (i)   : impulse response of weighted synthesis filter
                             h[-L_subfr..-1] must be set to zero           */
    Word16 cod[], /* (o)   : algebraic (fixed) codebook excitation         */
    Word16 y[],   /* (o)   : filtered fixed codebook excitation            */
    Word16 indx[] /* (o)   : index of 10 pulses (sign + position)          */
)
{
    Word16 ipos[NB_PULSE], pos_max[NB_TRACK], codvec[NB_PULSE];
    Word16 dn[L_CODE], sign[L_CODE];
    Word16 rr[L_CODE][L_CODE], i;

    cor_h_x (h, x, dn, 2);
    set_sign12k2 (dn, cn, sign, pos_max, NB_TRACK, ipos, STEP);
    cor_h (h, sign, rr);

    search_10and8i40 (NB_PULSE, STEP, NB_TRACK,
                      dn, rr, ipos, pos_max, codvec);
   
    build_code (codvec, sign, cod, h, y, indx);
    for (i = 0; i < 10; i++)
    {
        q_p (&indx[i], i);
    }
    return;
}

