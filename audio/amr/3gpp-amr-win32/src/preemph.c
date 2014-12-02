/*
********************************************************************************
*
*      GSM AMR-NB speech codec   R98   Version 7.6.0   December 12, 2001
*                                R99   Version 3.3.0                
*                                REL-4 Version 4.1.0                
*
********************************************************************************
*
*      File             : preemph.c
*      Purpose          : Preemphasis filtering
*      Description      : Filtering through 1 - g z^-1 
*
********************************************************************************
*/


/*
********************************************************************************
*                         MODULE INCLUDE FILE AND VERSION ID
********************************************************************************
*/
#include "preemph.h"
const char preemph_id[] = "@(#)$Id $" preemph_h;

/*
********************************************************************************
*                         INCLUDE FILES
********************************************************************************
*/
#include <stdlib.h>
#include <stdio.h>
#include "typedef.h"
#include "basic_op.h"
#include "count.h"

/*
********************************************************************************
*                         LOCAL VARIABLES AND TABLES
********************************************************************************
*/

/*
********************************************************************************
*                         PUBLIC PROGRAM CODE
********************************************************************************
*/
/*************************************************************************
*
*  Function:   Post_Filter_init
*  Purpose:    Allocates memory for filter structure and initializes
*              state memory
*
**************************************************************************
*/
int preemphasis_init (preemphasisState **state)
{
  preemphasisState* s;
 
  if (state == (preemphasisState **) NULL){
      fprintf(stderr, "preemphasis_init: invalid parameter\n");
      return -1;
  }
  *state = NULL;
 
  /* allocate memory */
  if ((s= (preemphasisState *) malloc(sizeof(preemphasisState))) == NULL){
      fprintf(stderr, "preemphasis_init: can not malloc state structure\n");
      return -1;
  }
  
  preemphasis_reset(s);
  *state = s;
  
  return 0;
}

/*************************************************************************
*
*  Function:   preemphasis_reset
*  Purpose:    Initializes state memory to zero
*
**************************************************************************
*/
int preemphasis_reset (preemphasisState *state)
{
  if (state == (preemphasisState *) NULL){
      fprintf(stderr, "preemphasis_reset: invalid parameter\n");
      return -1;
  }
  
  state->mem_pre = 0;
 
  return 0;
}
 
/*************************************************************************
*
*  Function:   preemphasis_exit
*  Purpose:    The memory used for state memory is freed
*
**************************************************************************
*/
void preemphasis_exit (preemphasisState **state)
{
  if (state == NULL || *state == NULL)
      return;
 
  /* deallocate memory */
  free(*state);
  *state = NULL;
  
  return;
}
 
/*
**************************************************************************
*  Function:  preemphasis
*  Purpose:   Filtering through 1 - g z^-1 
*
**************************************************************************
*/
int preemphasis (
    preemphasisState *st, /* (i/o)  : preemphasis filter state    */
    Word16 *signal, /* (i/o)   : input signal overwritten by the output */
    Word16 g,       /* (i)     : preemphasis coefficient                */
    Word16 L        /* (i)     : size of filtering                      */
)
{
    Word16 *p1, *p2, temp, i;

    p1 = signal + L - 1;                    move16 (); 
    p2 = p1 - 1;                            move16 (); 
    temp = *p1;                             move16 (); 

    for (i = 0; i <= L - 2; i++)
    {
        *p1 = sub (*p1, mult (g, *p2--));   move16 (); 
        p1--;
    }

    *p1 = sub (*p1, mult (g, st->mem_pre));     move16 (); 

    st->mem_pre = temp;                         move16 (); 

    return 0;
}
