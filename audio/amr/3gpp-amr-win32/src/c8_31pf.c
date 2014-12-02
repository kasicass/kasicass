/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : c8_31pf.c
*      Purpose          : Searches a 31 bit algebraic codebook containing 
*                       : 8 pulses in a frame of 40 samples.
*                       : in the same manner as GSM-EFR
*
********************************************************************************
*/
/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "c8_31pf.h"
const char c8_31pf_id[] = "@(#)$Id $" c8_31pf_h;
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
#include "cor_h.h"
#include "set_sign.h"
#include "s10_8pf.h"
#include <stdio.h>
#include <stdlib.h>

/*
********************************************************************************
*                         LOCAL VARIABLES AND TABLES
********************************************************************************
*/
#define NB_PULSE 8

/* define values/representation for output codevector and sign */
#define POS_CODE  8191 
#define NEG_CODE  8191 
#define POS_SIGN  32767
#define NEG_SIGN  (Word16) (-32768L) 

/*
********************************************************************************
*                         LOCAL PROGRAM CODE
********************************************************************************
*/

/*************************************************************************
 *
 *  FUNCTION:  build_code()
 *
 *  PURPOSE: Builds the codeword, the filtered codeword and a 
 *   linear uncombined version of  the index of the
 *           codevector, based on the signs and positions of 8  pulses.
 *
 *************************************************************************/

static void build_code (
    Word16 codvec[],    /* i : position of pulses                           */
    Word16 sign[],      /* i : sign of d[n]                                 */
    Word16 cod[],       /* o : innovative code vector                       */
    Word16 h[],         /* i : impulse response of weighted synthesis filter*/
    Word16 y[],         /* o : filtered innovative code                     */
    Word16 sign_indx[], /* o : signs of 4  pulses (signs only)              */
    Word16 pos_indx[]   /* o : position index of 8 pulses(position only)    */
)
{
    Word16 i, j, k, track, sign_index, pos_index, _sign[NB_PULSE];
    Word16 *p0, *p1, *p2, *p3, *p4, *p5, *p6, *p7;
    Word32 s;

    for (i = 0; i < L_CODE; i++)
    {
        cod[i] = 0;                              move16 (); 
    }
    for (i = 0; i < NB_TRACK_MR102; i++)
    {
        pos_indx[i] = -1;                            move16 (); 
        sign_indx[i] = -1;                            move16 (); 
    }
    
    for (k = 0; k < NB_PULSE; k++)
    {
       /* read pulse position */            
       i = codvec[k];                           move16 ();
       /* read sign           */        
       j = sign[i];                             move16 (); 
       
       pos_index = shr(i, 2);                      /* index = pos/4 */
       track = i & 3;             logic16 ();      /* track = pos%4 */
       
       test (); 
       if (j > 0)
       {
          cod[i] = add (cod[i], POS_CODE);         move16 ();
          _sign[k] = POS_SIGN;                     move16 (); 
          sign_index = 0;  /* bit=0 -> positive pulse */  move16 (); 
       }
       else
       {
          cod[i] = sub (cod[i], NEG_CODE);         move16 ();
          _sign[k] = NEG_SIGN;                     move16 (); 
          sign_index = 1;     move16 (); /* bit=1 => negative pulse */ 
          /* index = add (index, 8); 1 = negative  old code */
       }
       
       test (); move16 ();
       if (pos_indx[track] < 0)
       {   /* first set first NB_TRACK pulses  */
          pos_indx[track] = pos_index;                 move16 (); 
          sign_indx[track] = sign_index;              move16 (); 
       }
       else
       {   /* 2nd row of pulses , test if positions needs to be switched */
          test (); logic16 (); logic16 (); 
          if (((sign_index ^ sign_indx[track]) & 1) == 0)
          {
             /* sign of 1st pulse == sign of 2nd pulse */
             
             test (); 
             if (sub (pos_indx[track], pos_index) <= 0)
             {   /* no swap */
                pos_indx[track + NB_TRACK_MR102] = pos_index;     move16 (); 
             }
             else
             {   /* swap*/
                pos_indx[track + NB_TRACK_MR102] = pos_indx[track];
                move16 (); 
                
                pos_indx[track] = pos_index;         move16 ();
                sign_indx[track] = sign_index;       move16 ();
             }
          }
          else
          {
             /* sign of 1st pulse != sign of 2nd pulse */
             
             test (); 
             if (sub (pos_indx[track], pos_index) <= 0)
             {  /*swap*/
                pos_indx[track + NB_TRACK_MR102] = pos_indx[track];
                move16 (); 
                
                pos_indx[track] = pos_index;         move16 (); 
                sign_indx[track] = sign_index;       move16 (); 
             }
             else
             {   /*no swap */
                pos_indx[track + NB_TRACK_MR102] = pos_index;     move16 (); 
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
       y[i] = round (s);                        move16 (); 
    }
}

/*************************************************************************
 *
 *  FUNCTION:  compress_code()
 *
 *  PURPOSE: compression of three indeces [0..9] to one 10 bit index   
 *           minimizing the phase shift of a bit error.
 *
 *************************************************************************/


static Word16 compress10 (
       Word16 pos_indxA, /* i : signs of 4 pulses (signs only)             */
       Word16 pos_indxB,  /* i : position index of 8 pulses (pos only)     */
       Word16 pos_indxC) /* i : position and sign of 8 pulses (compressed) */
{
   Word16 indx, ia,ib,ic;

   ia = shr(pos_indxA, 1);
   ib = extract_l(L_shr(L_mult(shr(pos_indxB, 1), 5), 1));
   ic = extract_l(L_shr(L_mult(shr(pos_indxC, 1), 25), 1));            
   indx = shl(add(ia, add(ib, ic)), 3);
   ia = pos_indxA & 1;                                logic16 ();
   ib = shl((pos_indxB & 1), 1);                      logic16 ();
   ic = shl((pos_indxC & 1), 2);                      logic16 ();
   indx = add(indx , add(ia, add(ib, ic)));  
   
   return indx;

}

/*************************************************************************
 *
 *  FUNCTION:  compress_code()
 *
 *  PURPOSE: compression of the linear codewords to 4+three indeces  
 *           one bit from each pulse is made robust to errors by 
 *           minimizing the phase shift of a bit error.
 *           4 signs (one for each track) 
 *           i0,i4,i1 => one index (7+3) bits, 3   LSBs more robust
 *           i2,i6,i5 => one index (7+3) bits, 3   LSBs more robust
 *           i3,i7    => one index (5+2) bits, 2-3 LSbs more robust
 *
 *************************************************************************/


static void compress_code (
    Word16 sign_indx[], /* i : signs of 4 pulses (signs only)             */
    Word16 pos_indx[],  /* i : position index of 8 pulses (position only) */
    Word16 indx[])      /* o : position and sign of 8 pulses (compressed) */
{
   Word16 i, ia, ib, ic;

   for (i = 0; i < NB_TRACK_MR102; i++)
   {
      indx[i] = sign_indx[i];                            move16 (); 
   }
    
    /* First index 
      indx[NB_TRACK] = (ia/2+(ib/2)*5 +(ic/2)*25)*8 + ia%2 + (ib%2)*2 + (ic%2)*4; */
   move16 (); 
   indx[NB_TRACK_MR102] = compress10(pos_indx[0],pos_indx[4],pos_indx[1]);

    /* Second index       
      indx[NB_TRACK+1] = (ia/2+(ib/2)*5 +(ic/2)*25)*8 + ia%2 + (ib%2)*2 + (ic%2)*4; */
    
   move16 (); 
   indx[NB_TRACK_MR102+1]= compress10(pos_indx[2],pos_indx[6],pos_indx[5]);
    
    /*
      Third index      
      if ((ib/2)%2 == 1)
        indx[NB_TRACK+2] = ((((4-ia/2) + (ib/2)*5)*32+12)/25)*4 + ia%2 + (ib%2)*2;
      else   
        indx[NB_TRACK+2] = ((((ia/2) +   (ib/2)*5)*32+12)/25)*4 + ia%2 + (ib%2)*2;
        */
    
    ib = shr(pos_indx[7], 1) & 1;                        logic16 ();
    test ();
    if (sub(ib, 1) == 0)
       ia = sub(4, shr(pos_indx[3], 1));
    else
       ia = shr(pos_indx[3], 1);

    ib = extract_l(L_shr(L_mult(shr(pos_indx[7], 1), 5), 1));       
    ib = add(shl(add(ia, ib), 5), 12);
    ic = shl(mult(ib, 1311), 2);
    ia = pos_indx[3] & 1;                             logic16 ();
    ib = shl((pos_indx[7] & 1), 1);                   logic16 ();
    indx[NB_TRACK_MR102+2] = add(ia, add(ib, ic));
}


/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
/*************************************************************************
 *
 *  FUNCTION:  code_8i40_31bits()
 *
 *  PURPOSE:  Searches a 31 bit algebraic codebook containing 8 pulses
 *            in a frame of 40 samples.
 *
 *  DESCRIPTION:
 *    The code contains 8 nonzero pulses: i0...i7.
 *    All pulses can have two possible amplitudes: +1 or -1.
 *    The 40 positions in a subframe are divided into 4 tracks of
 *    interleaved positions. Each track contains two pulses.
 *    The pulses can have the following possible positions:
 *
 *       i0, i4 :  0, 4, 8,  12, 16, 20, 24, 28, 32, 36
 *       i1, i5 :  1, 5, 9,  13, 17, 21, 25, 29, 33, 37
 *       i2, i6 :  2, 6, 10, 14, 18, 22, 26, 30, 34, 38
 *       i3, i7 :  3, 7, 11, 15, 19, 23, 27, 31, 35, 39
 *
 *    Each pair of pulses require 1 bit for their signs. The positions 
 *    are encoded together 3,3 and 2 resulting in   
 *    (7+3) + (7+3) + (5+2) bits for their
 *    positions. This results in a 31 (4 sign and 27 pos) bit codebook.
 *    The function determines the optimal pulse signs and positions, builds
 *    the codevector, and computes the filtered codevector.
 *
 *************************************************************************/

void code_8i40_31bits (
   Word16 x[],        /* i : target vector                                  */
   Word16 cn[],       /* i : residual after long term prediction            */
   Word16 h[],        /* i : impulse response of weighted synthesis
                             filter                                         */
   Word16 cod[],      /* o : algebraic (fixed) codebook excitation          */
   Word16 y[],        /* o : filtered fixed codebook excitation             */
   Word16 indx[]      /* o : 7 Word16, index of 8 pulses (signs+positions)  */
)
{
   Word16 ipos[NB_PULSE], pos_max[NB_TRACK_MR102], codvec[NB_PULSE];
   Word16 dn[L_CODE], sign[L_CODE];
   Word16 rr[L_CODE][L_CODE];
   Word16 linear_signs[NB_TRACK_MR102];
   Word16 linear_codewords[NB_PULSE];

   cor_h_x2 (h, x, dn, 2, NB_TRACK_MR102, STEP_MR102);
   /* 2 = use GSMEFR scaling */

   set_sign12k2 (dn, cn, sign, pos_max, NB_TRACK_MR102, ipos, STEP_MR102);
   /* same setsign alg as GSM-EFR new constants though*/
   
   cor_h (h, sign, rr);
   search_10and8i40 (NB_PULSE, STEP_MR102, NB_TRACK_MR102,
                     dn, rr, ipos, pos_max, codvec);
   
   build_code (codvec, sign, cod, h, y, linear_signs, linear_codewords);
   compress_code (linear_signs, linear_codewords, indx);
   
   return;
}
