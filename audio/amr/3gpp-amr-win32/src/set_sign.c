/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : set_sign.c
*      Purpose          : Builds sign vector according to "dn[]" and "cn[]".
*
********************************************************************************
*/
/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "set_sign.h"
const char set_sign_id[] = "@(#)$Id $" set_sign_h;
/*
********************************************************************************
*                         INCLUDE FILES
********************************************************************************
*/
#include "typedef.h"
#include "basic_op.h"
#include "count.h"
#include "inv_sqrt.h"
#include "cnst.h" 
 
/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/

/*************************************************************************
 *
 *  FUNCTION  set_sign()
 *
 *  PURPOSE: Builds sign[] vector according to "dn[]" and "cn[]".
 *           Also finds the position of maximum of correlation in each track
 *           and the starting position for each pulse.
 *
 *************************************************************************/
void set_sign(Word16 dn[],   /* i/o : correlation between target and h[]    */
              Word16 sign[], /* o   : sign of dn[]                          */
              Word16 dn2[],  /* o   : maximum of correlation in each track. */
              Word16 n       /* i   : # of maximum correlations in dn2[]    */
)
{
   Word16 i, j, k;
   Word16 val, min;
   Word16 pos = 0; /* initialization only needed to keep gcc silent */
   
   /* set sign according to dn[] */
   
   for (i = 0; i < L_CODE; i++) {
      val = dn[i];                                 move16 ();
      
      test ();
      if (val >= 0) {
         sign[i] = 32767;                          move16 ();
      } else {
         sign[i] = -32767;                         move16 ();
         val = negate(val);
      }
      dn[i] = val;    move16 (); /* modify dn[] according to the fixed sign */
      dn2[i] = val;   move16 ();
   }
   
   /* keep 8-n maximum positions/8 of each track and store it in dn2[] */
   
   for (i = 0; i < NB_TRACK; i++)
   {
      for (k = 0; k < (8-n); k++)
      {
         min = 0x7fff;                             move16 ();
         for (j = i; j < L_CODE; j += STEP)
         {
            test ();                               move16 ();
            if (dn2[j] >= 0)
            {
               val = sub(dn2[j], min);
               test ();
               if (val < 0)
               {
                  min = dn2[j];                    move16 ();
                  pos = j;                         move16 ();
               }
            }
         }
         dn2[pos] = -1;                            move16 ();
      }
   }
   
   return;
}

/*************************************************************************
 *
 *  FUNCTION  set_sign12k2()
 *
 *  PURPOSE: Builds sign[] vector according to "dn[]" and "cn[]", and modifies
 *           dn[] to include the sign information (dn[i]=sign[i]*dn[i]).
 *           Also finds the position of maximum of correlation in each track
 *           and the starting position for each pulse.
 *
 *************************************************************************/
void set_sign12k2 (
    Word16 dn[],      /* i/o : correlation between target and h[]         */
    Word16 cn[],      /* i   : residual after long term prediction        */
    Word16 sign[],    /* o   : sign of d[n]                               */
    Word16 pos_max[], /* o   : position of maximum correlation            */
    Word16 nb_track,  /* i   : number of tracks tracks                    */        
    Word16 ipos[],    /* o   : starting position for each pulse           */
    Word16 step       /* i   : the step size in the tracks                */        
)
{
    Word16 i, j;
    Word16 val, cor, k_cn, k_dn, max, max_of_all;
    Word16 pos = 0; /* initialization only needed to keep gcc silent */
    Word16 en[L_CODE];                  /* correlation vector */
    Word32 s;
 
    /* calculate energy for normalization of cn[] and dn[] */
 
    s = 256;                                     move32 (); 
    for (i = 0; i < L_CODE; i++)
    {
        s = L_mac (s, cn[i], cn[i]);
    }
    s = Inv_sqrt (s);                            move32 (); 
    k_cn = extract_h (L_shl (s, 5));
    
    s = 256;                                     move32 (); 
    for (i = 0; i < L_CODE; i++)
    {
        s = L_mac (s, dn[i], dn[i]);
    }
    s = Inv_sqrt (s);                            move32 (); 
    k_dn = extract_h (L_shl (s, 5));
    
    for (i = 0; i < L_CODE; i++)
    {
        val = dn[i];                             move16 (); 
        cor = round (L_shl (L_mac (L_mult (k_cn, cn[i]), k_dn, val), 10));
 
        test (); 
        if (cor >= 0)
        {
            sign[i] = 32767;                     move16 (); /* sign = +1 */
        }
        else
        {
            sign[i] = -32767;                    move16 (); /* sign = -1 */
            cor = negate (cor);
            val = negate (val);
        }
        /* modify dn[] according to the fixed sign */        
        dn[i] = val;                             move16 (); 
        en[i] = cor;                             move16 (); 
    }
    
    max_of_all = -1;                             move16 (); 
    for (i = 0; i < nb_track; i++)
    {
        max = -1;                                move16 (); 
        
        for (j = i; j < L_CODE; j += step)
        {
            cor = en[j];                         move16 (); 
            val = sub (cor, max);
            test (); 
            if (val > 0)
            {
                max = cor;                       move16 (); 
                pos = j;                         move16 (); 
            }
        }
        /* store maximum correlation position */
        pos_max[i] = pos;                        move16 (); 
        val = sub (max, max_of_all);
        test (); 
        if (val > 0)
        {
            max_of_all = max;                    move16 ();
            /* starting position for i0 */            
            ipos[0] = i;                         move16 (); 
        }
    }
    
    /*----------------------------------------------------------------*
     *     Set starting position of each pulse.                       *
     *----------------------------------------------------------------*/
    
    pos = ipos[0];                               move16 (); 
    ipos[nb_track] = pos;                        move16 (); 
    
    for (i = 1; i < nb_track; i++)
    {
        pos = add (pos, 1);
        test ();
        if (sub (pos, nb_track) >= 0)
        {
           pos = 0;                              move16 (); 
        }
        ipos[i] = pos;                           move16 (); 
        ipos[add(i, nb_track)] = pos;            move16 (); 
    }
}
