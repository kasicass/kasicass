/*
*****************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
****************************************************************************
*
*      File             : q_plsf_3.c
*      Purpose          : Quantization of LSF parameters with 1st order MA
*                         prediction and split by 3 vector quantization
*                         (split-VQ)
*
*****************************************************************************
*/

/*
*****************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
*****************************************************************************
*/
#include "q_plsf.h"
const char q_plsf_3_id[] = "@(#)$Id $" q_plsf_h;
 
/*
*****************************************************************************
*                         INCLUDE FILES
*****************************************************************************
*/
#include <stdlib.h>
#include <stdio.h>
#include "typedef.h"
#include "basic_op.h"
#include "count.h"
#include "lsp_lsf.h"
#include "reorder.h"
#include "lsfwt.h"
#include "copy.h"

/*
*****************************************************************************
*                         LOCAL VARIABLES AND TABLES
*****************************************************************************
*/
#include "q_plsf_3.tab"        /* Codebooks of LSF prediction residual */
#define PAST_RQ_INIT_SIZE 8
/*
*****************************************************************************
*                         LOCAL PROGRAM CODE
*****************************************************************************
*/
/* Quantization of a 4 dimensional subvector */

static Word16
Vq_subvec4(             /* o: quantization index,            Q0  */
    Word16 * lsf_r1,    /* i: 1st LSF residual vector,       Q15 */
    Word16 * dico,      /* i: quantization codebook,         Q15 */
    Word16 * wf1,       /* i: 1st LSF weighting factors,     Q13 */
    Word16 dico_size)   /* i: size of quantization codebook, Q0  */  
{
    Word16 i, index = 0;
    Word16 *p_dico, temp;
    Word32 dist_min, dist;

    dist_min = MAX_32;                                              move32 ();
    p_dico = dico;                                                  move16 ();

    for (i = 0; i < dico_size; i++)
    {
        temp = sub (lsf_r1[0], *p_dico++);
        temp = mult (wf1[0], temp);
        dist = L_mult (temp, temp);

        temp = sub (lsf_r1[1], *p_dico++);
        temp = mult (wf1[1], temp);
        dist = L_mac (dist, temp, temp);

        temp = sub (lsf_r1[2], *p_dico++);
        temp = mult (wf1[2], temp);
        dist = L_mac (dist, temp, temp);

        temp = sub (lsf_r1[3], *p_dico++);
        temp = mult (wf1[3], temp);
        dist = L_mac (dist, temp, temp);

        test ();
		if (L_sub (dist, dist_min) < (Word32) 0)
        {
            dist_min = dist;                                       move32 ();
            index = i;                                             move16 ();
        }
    }

    /* Reading the selected vector */

    p_dico = &dico[shl (index, 2)];                                move16 ();
    lsf_r1[0] = *p_dico++;                                         move16 ();
    lsf_r1[1] = *p_dico++;                                         move16 ();
    lsf_r1[2] = *p_dico++;                                         move16 ();
    lsf_r1[3] = *p_dico++;                                         move16 ();

    return index;

}

/* Quantization of a 3 dimensional subvector */

static Word16
Vq_subvec3(             /* o: quantization index,            Q0  */
    Word16 * lsf_r1,    /* i: 1st LSF residual vector,       Q15 */
    Word16 * dico,      /* i: quantization codebook,         Q15 */
    Word16 * wf1,       /* i: 1st LSF weighting factors,     Q13 */
    Word16 dico_size,   /* i: size of quantization codebook, Q0  */
    Flag use_half)      /* i: use every second entry in codebook */
{
    Word16 i, index = 0;
    Word16 *p_dico, temp;
    Word32 dist_min, dist;

    dist_min = MAX_32;                                             move32 ();
    p_dico = dico;                                                 move16 ();

    test ();
    if (use_half == 0) {
       for (i = 0; i < dico_size; i++)
       {
          temp = sub(lsf_r1[0], *p_dico++);
          temp = mult(wf1[0], temp);
          dist = L_mult(temp, temp);
          
          temp = sub(lsf_r1[1], *p_dico++);
          temp = mult(wf1[1], temp);
          dist = L_mac(dist, temp, temp);
          
          temp = sub(lsf_r1[2], *p_dico++);
          temp = mult(wf1[2], temp);
          dist = L_mac(dist, temp, temp);

          test ();
          if (L_sub(dist, dist_min) < (Word32) 0) {
             dist_min = dist;                                   move32 ();
             index = i;                                         move16 ();
          }
       }
       p_dico = &dico[add(index, add(index, index))];             move16 ();
    }
    else
    {
       for (i = 0; i < dico_size; i++)
       {
          temp = sub(lsf_r1[0], *p_dico++);
          temp = mult(wf1[0], temp);
          dist = L_mult(temp, temp);

          temp = sub(lsf_r1[1], *p_dico++);
          temp = mult(wf1[1], temp);
          dist = L_mac(dist, temp, temp);
          
          temp = sub(lsf_r1[2], *p_dico++);
          temp = mult(wf1[2], temp);
          dist = L_mac(dist, temp, temp);
          
          test ();
          if (L_sub(dist, dist_min) < (Word32) 0)
          {
             dist_min = dist;                                   move32 ();
             index = i;                                         move16 ();
          }
          p_dico = p_dico + 3; add(0,0);
       }
       p_dico = &dico[shl(add(index, add(index, index)),1)];      move16 ();
    }
    

    /* Reading the selected vector */
    lsf_r1[0] = *p_dico++;                                         move16 ();
    lsf_r1[1] = *p_dico++;                                         move16 ();
    lsf_r1[2] = *p_dico++;                                         move16 ();

    return index;
}

/*
*****************************************************************************
*                         PUBLIC PROGRAM CODE
*****************************************************************************
*/

/***********************************************************************
 *
 * routine:   Q_plsf_3()
 *
 * Quantization of LSF parameters with 1st order MA prediction and
 * split by 3 vector quantization (split-VQ)
 *
 ***********************************************************************/
void Q_plsf_3(
    Q_plsfState *st,    /* i/o: state struct                             */
    enum Mode mode,     /* i  : coder mode                               */
    Word16 *lsp1,       /* i  : 1st LSP vector                      Q15  */
    Word16 *lsp1_q,     /* o  : quantized 1st LSP vector            Q15  */
    Word16 *indice,     /* o  : quantization indices of 3 vectors   Q0   */
    Word16 *pred_init_i /* o  : init index for MA prediction in DTX mode */
)
{
    Word16 i, j;
    Word16 lsf1[M], wf1[M], lsf_p[M], lsf_r1[M];
    Word16 lsf1_q[M];
    
    Word32 L_pred_init_err;
    Word32 L_min_pred_init_err;
    Word16 temp_r1[M];
    Word16 temp_p[M];

    /* convert LSFs to normalize frequency domain 0..16384 */

    Lsp_lsf(lsp1, lsf1, M);

    /* compute LSF weighting factors (Q13) */

    Lsf_wt(lsf1, wf1);

    /* Compute predicted LSF and prediction error */
    if (test(), sub(mode, MRDTX) != 0)
    {
       for (i = 0; i < M; i++)
       {
          lsf_p[i] = add(mean_lsf[i], 
                         mult(st->past_rq[i], 
                              pred_fac[i]));                            move16 ();
          lsf_r1[i] = sub(lsf1[i], lsf_p[i]);                           move16 ();
      }
    }
    else
    {
       /* DTX mode, search the init vector that yields */
       /* lowest prediction resuidual energy           */
       *pred_init_i = 0;                                                move16 ();
       L_min_pred_init_err = 0x7fffffff; /* 2^31 - 1 */                 move32 ();
       for (j = 0; j < PAST_RQ_INIT_SIZE; j++)
       {
          L_pred_init_err = 0;                                          move32 ();
          for (i = 0; i < M; i++)
          {
             temp_p[i] = add(mean_lsf[i], past_rq_init[j*M+i]);
             temp_r1[i] = sub(lsf1[i],temp_p[i]);
             L_pred_init_err = L_mac(L_pred_init_err, temp_r1[i], temp_r1[i]);
          }  /* next i */

          test ();
          if (L_sub(L_pred_init_err, L_min_pred_init_err) < (Word32) 0)
          {
             L_min_pred_init_err = L_pred_init_err;                     move32 (); 
             Copy(temp_r1, lsf_r1, M);
             Copy(temp_p, lsf_p, M);
             
             /* Set zerom */
             Copy(&past_rq_init[j*M], st->past_rq, M);
             *pred_init_i = j;                                          move16 ();            
          } /* endif */
       } /* next j */
    } /* endif MRDTX */
    
    /*---- Split-VQ of prediction error ----*/
    if (sub (mode, MR475) == 0 || sub (mode, MR515) == 0)
    {   /* MR475, MR515 */
      test (); test (); 
                          
      indice[0] = Vq_subvec3(&lsf_r1[0], dico1_lsf, &wf1[0], DICO1_SIZE, 0);
      move16 ();
      indice[1] = Vq_subvec3(&lsf_r1[3], dico2_lsf, &wf1[3], DICO2_SIZE/2, 1);
      move16 ();
      indice[2] = Vq_subvec4(&lsf_r1[6], mr515_3_lsf, &wf1[6], MR515_3_SIZE);
      move16 ();
    }
    else if (sub (mode, MR795) == 0)
    {   /* MR795 */
      test (); test (); test ();
       
      indice[0] = Vq_subvec3(&lsf_r1[0], mr795_1_lsf, &wf1[0], MR795_1_SIZE, 0);
      move16 ();
      indice[1] = Vq_subvec3(&lsf_r1[3], dico2_lsf, &wf1[3], DICO2_SIZE, 0);
      move16 ();
      indice[2] = Vq_subvec4(&lsf_r1[6], dico3_lsf, &wf1[6], DICO3_SIZE);
      move16 ();
    }
    else 
    {   /* MR59, MR67, MR74, MR102 , MRDTX */
      test (); test (); test ();

      indice[0] = Vq_subvec3(&lsf_r1[0], dico1_lsf, &wf1[0], DICO1_SIZE, 0);
      move16 ();
      indice[1] = Vq_subvec3(&lsf_r1[3], dico2_lsf, &wf1[3], DICO2_SIZE, 0);
      move16 ();
      indice[2] = Vq_subvec4(&lsf_r1[6], dico3_lsf, &wf1[6], DICO3_SIZE);
      move16 ();
    }

        
    /* Compute quantized LSFs and update the past quantized residual */

    for (i = 0; i < M; i++)
    {
        lsf1_q[i] = add(lsf_r1[i], lsf_p[i]);                        move16 ();
        st->past_rq[i] = lsf_r1[i];                                  move16 ();
    }

    /* verification that LSFs has mimimum distance of LSF_GAP Hz */

    Reorder_lsf(lsf1_q, LSF_GAP, M);

    /*  convert LSFs to the cosine domain */

    Lsf_lsp(lsf1_q, lsp1_q, M);
}
