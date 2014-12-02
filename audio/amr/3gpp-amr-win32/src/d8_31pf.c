/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : d8_31pf.c
*      Purpose          : Builds the innovative codevector
*
********************************************************************************
*/
 
/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "d8_31pf.h"
const char d8_31pf_id[] = "@(#)$Id $" d8_31pf_h;
 
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
#define NB_PULSE  8           /* number of pulses  */

/* define values/representation for output codevector and sign */
#define POS_CODE  8191 
#define NEG_CODE  8191 

static void decompress10 (
   Word16 MSBs,        /* i : MSB part of the index                 */
   Word16 LSBs,        /* i : LSB part of the index                 */
   Word16 index1,      /* i : index for first pos in pos_index[]    */ 
   Word16 index2,      /* i : index for second pos in pos_index[]   */ 
   Word16 index3,      /* i : index for third pos in pos_index[]    */ 
   Word16 pos_indx[])  /* o : position of 3 pulses (decompressed)   */
{
   Word16 ia, ib, ic;

   /*
     pos_indx[index1] = ((MSBs-25*(MSBs/25))%5)*2 + (LSBs-4*(LSBs/4))%2;
     pos_indx[index2] = ((MSBs-25*(MSBs/25))/5)*2 + (LSBs-4*(LSBs/4))/2;
     pos_indx[index3] = (MSBs/25)*2 + LSBs/4;
     */

   test ();
   if (sub(MSBs, 124) > 0)
   {
      MSBs = 124;                                              move16 (); 
   }
   
   ia = mult(MSBs, 1311);
   ia = sub(MSBs, extract_l(L_shr(L_mult(ia, 25), 1)));    
   ib = shl(sub(ia, extract_l(L_shr(L_mult(mult(ia, 6554), 5), 1))), 1);
   
   ic = shl(shr(LSBs, 2), 2);
   ic = sub(LSBs, ic);
   pos_indx[index1] = add(ib, (ic & 1));                        logic16 ();
   
   ib = shl(mult(ia, 6554), 1);
   pos_indx[index2] = add(ib, shr(ic, 1));
   
   pos_indx[index3] = add(shl(mult(MSBs, 1311), 1), shr(LSBs, 2));    

   return;
}    

/*************************************************************************
 *
 *  FUNCTION:  decompress_code()
 *
 *  PURPOSE: decompression of the linear codewords to 4+three indeces  
 *           one bit from each pulse is made robust to errors by 
 *           minimizing the phase shift of a bit error.
 *           4 signs (one for each track) 
 *           i0,i4,i1 => one index (7+3) bits, 3   LSBs more robust
 *           i2,i6,i5 => one index (7+3) bits, 3   LSBs more robust
 *           i3,i7    => one index (5+2) bits, 2-3 LSbs more robust
 *
 *************************************************************************/
static void decompress_code (
    Word16 indx[],      /* i : position and sign of 8 pulses (compressed) */
    Word16 sign_indx[], /* o : signs of 4 pulses (signs only)             */
    Word16 pos_indx[]   /* o : position index of 8 pulses (position only) */
)
{
    Word16 i, ia, ib, MSBs, LSBs, MSBs0_24;

    for (i = 0; i < NB_TRACK_MR102; i++)
    {
       sign_indx[i] = indx[i];                                  move16 (); 
    }
    
    /*
      First index: 10x10x10 -> 2x5x2x5x2x5-> 125x2x2x2 -> 7+1x3 bits 
      MSBs = indx[NB_TRACK]/8;
      LSBs = indx[NB_TRACK]%8;
      */
    MSBs = shr(indx[NB_TRACK_MR102], 3);
    LSBs = indx[NB_TRACK_MR102] & 7;                            logic16 ();
    decompress10 (MSBs, LSBs, 0, 4, 1, pos_indx);               
    
    /*
      Second index: 10x10x10 -> 2x5x2x5x2x5-> 125x2x2x2 -> 7+1x3 bits       
      MSBs = indx[NB_TRACK+1]/8;
      LSBs = indx[NB_TRACK+1]%8;
      */
    MSBs = shr(indx[NB_TRACK_MR102+1], 3);
    LSBs = indx[NB_TRACK_MR102+1] & 7;                          logic16 ();
    decompress10 (MSBs, LSBs, 2, 6, 5, pos_indx);               
    
    /*
      Third index: 10x10 -> 2x5x2x5-> 25x2x2 -> 5+1x2 bits    
      MSBs = indx[NB_TRACK+2]/4;
      LSBs = indx[NB_TRACK+2]%4;
      MSBs0_24 = (MSBs*25+12)/32;
      if ((MSBs0_24/5)%2==1)
         pos_indx[3] = (4-(MSBs0_24%5))*2 + LSBs%2;
      else
         pos_indx[3] = (MSBs0_24%5)*2 + LSBs%2;
      pos_indx[7] = (MSBs0_24/5)*2 + LSBs/2;
      */
    MSBs = shr(indx[NB_TRACK_MR102+2], 2);
    LSBs = indx[NB_TRACK_MR102+2] & 3;                          logic16 ();

    MSBs0_24 = shr(add(extract_l(L_shr(L_mult(MSBs, 25), 1)), 12), 5);
    
    ia = mult(MSBs0_24, 6554) & 1;
    ib = sub(MSBs0_24, extract_l(L_shr(L_mult(mult(MSBs0_24, 6554), 5), 1)));

    test ();
    if (sub(ia, 1) == 0)
    {
       ib = sub(4, ib);
    }
    pos_indx[3] = add(shl(ib, 1), (LSBs & 1));               logic16 ();           
    
    ia = shl(mult(MSBs0_24, 6554), 1);
    pos_indx[7] = add(ia, shr(LSBs, 1));
}

/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
/*************************************************************************
 *
 *  FUNCTION:   dec_8i40_31bits()
 *
 *  PURPOSE:  Builds the innovative codevector from the received
 *            index of algebraic codebook.
 *
 *   See  c8_31pf.c  for more details about the algebraic codebook structure.
 *
 *************************************************************************/

void dec_8i40_31bits (
    Word16 index[],    /* i : index of 8 pulses (sign+position)         */
    Word16 cod[]       /* o : algebraic (fixed) codebook excitation     */
)
{
    Word16 i, j, pos1, pos2, sign;
    Word16 linear_signs[NB_TRACK_MR102];
    Word16 linear_codewords[NB_PULSE];
    
    for (i = 0; i < L_CODE; i++)
    {
        cod[i] = 0;                                    move16 (); 
    }
    
    decompress_code (index, linear_signs, linear_codewords);
    
    /* decode the positions and signs of pulses and build the codeword */

    for (j = 0; j < NB_TRACK_MR102; j++)
    {
       /* compute index i */
       
       i = linear_codewords[j];
       i = extract_l (L_shr (L_mult (i, 4), 1));
       pos1 = add (i, j);   /* position of pulse "j" */
       
       test (); 
       if (linear_signs[j] == 0)
       {
          sign = POS_CODE;                             move16 (); /* +1.0 */
       }
       else
       {
          sign = -NEG_CODE;                            move16 (); /* -1.0 */
       }
       
       cod[pos1] = sign;                               move16 (); 
       
       /* compute index i */
       
       i = linear_codewords[add (j, 4)];        
       i = extract_l (L_shr (L_mult (i, 4), 1));
       pos2 = add (i, j);      /* position of pulse "j+4" */
       
       test (); 
       if (sub (pos2, pos1) < 0)
       {
          sign = negate (sign);
       }
       cod[pos2] = add (cod[pos2], sign);              move16 (); 
    }
    
    return;
}
