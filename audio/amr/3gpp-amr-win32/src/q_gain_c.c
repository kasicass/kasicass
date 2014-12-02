/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : q_gain_c.c
*      Purpose          : Scalar quantization of the innovative
*                       : codebook gain.
*
********************************************************************************
*/
 
 
/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "q_gain_c.h"
const char q_gain_c_id[] = "@(#)$Id $" q_gain_c_h;
 
/*
********************************************************************************
*                         INCLUDE FILES
********************************************************************************
*/
#include <stdlib.h>
#include <stdio.h>
#include "typedef.h"
#include "mode.h"
#include "basic_op.h"
#include "oper_32b.h"
#include "count.h"
#include "log2.h"
#include "pow2.h"

/*
********************************************************************************
*                         LOCAL VARIABLES AND TABLES
********************************************************************************
*/
#include "gains.tab"

/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
 
/*--------------------------------------------------------------------------*
 * Function q_gain_code()                                                   *
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                                  *
 * Scalar quantization of the innovative codebook gain.                     *
 *                                                                          *
 *--------------------------------------------------------------------------*/
Word16 q_gain_code (        /* o  : quantization index,            Q0  */
    enum Mode mode,         /* i  : AMR mode                           */
    Word16 exp_gcode0,      /* i  : predicted CB gain (exponent),  Q0  */
    Word16 frac_gcode0,     /* i  : predicted CB gain (fraction),  Q15 */
    Word16 *gain,           /* i/o: quantized fixed codebook gain, Q1  */
    Word16 *qua_ener_MR122, /* o  : quantized energy error,        Q10 */
                            /*      (for MR122 MA predictor update)    */
    Word16 *qua_ener        /* o  : quantized energy error,        Q10 */
                            /*      (for other MA predictor update)    */
)
{
    const Word16 *p;
    Word16 i, index;
    Word16 gcode0, err, err_min;
    Word16 g_q0;

    test ();
    g_q0 = 0;    move16 ();
    test ();
    if (sub(mode, MR122) == 0)
    {
       g_q0 = shr (*gain, 1); /* Q1 -> Q0 */
    }

    /*-------------------------------------------------------------------*
     *  predicted codebook gain                                          *
     *  ~~~~~~~~~~~~~~~~~~~~~~~                                          *
     *  gc0     = Pow2(int(d)+frac(d))                                   *
     *          = 2^exp + 2^frac                                         *
     *                                                                   *
     *-------------------------------------------------------------------*/

    gcode0 = extract_l (Pow2 (exp_gcode0, frac_gcode0));  /* predicted gain */

    test ();
    if (sub(mode, MR122) == 0)
    {
       gcode0 = shl (gcode0, 4);
    }
    else
    {
       gcode0 = shl (gcode0, 5);
    }
    
    /*-------------------------------------------------------------------*
     *                   Search for best quantizer                        *
     *-------------------------------------------------------------------*/

    p = &qua_gain_code[0]; move16 ();
    test ();
    if (sub(mode, MR122) == 0)
    {
       err_min = abs_s (sub (g_q0, mult (gcode0, *p++)));
    }
    else
    {
       err_min = abs_s (sub (*gain, mult (gcode0, *p++)));
    }
    p += 2;                                  /* skip quantized energy errors */
    index = 0;              move16 (); 

    for (i = 1; i < NB_QUA_CODE; i++)
    {
       test ();
       if (sub(mode, MR122) == 0)
       {
          err = abs_s (sub (g_q0,  mult (gcode0, *p++)));
       }
       else
       {
          err = abs_s (sub (*gain, mult (gcode0, *p++)));
       }
       
       p += 2;                              /* skip quantized energy error */

       test (); 
       if (sub (err, err_min) < 0)
       {
          err_min = err;                  move16 (); 
          index = i;                      move16 (); 
       }
    }

    p = &qua_gain_code[add (add (index,index), index)]; move16 ();
    test ();
    if (sub(mode, MR122) == 0)
    {
       *gain = shl (mult (gcode0, *p++), 1); 
    }
    else
    {
       *gain = mult (gcode0, *p++);
    }
                                            move16 ();
    
    /* quantized error energies (for MA predictor update) */
    *qua_ener_MR122 = *p++;                 move16 (); 
    *qua_ener = *p;                         move16 (); 

    return index;
}
