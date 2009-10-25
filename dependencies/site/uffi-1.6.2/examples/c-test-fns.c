/***************************************************************************
 * FILE IDENTIFICATION
 *
 *  Name:         c-test-fns.c
 *  Purpose:      Test functions in C for UFFI library
 *  Programer:    Kevin M. Rosenberg
 *  Date Started: Mar 2002
 *
 *  CVS Id:   $Id$
 *
 * This file, part of UFFI, is Copyright (c) 2002-2005 by Kevin M. Rosenberg
 *
 * These variables are correct for GCC
 * you'll need to modify these for other compilers
 ***************************************************************************/

#if defined(WIN32)||defined(WIN64)
#include <windows.h>

BOOL WINAPI DllEntryPoint(HINSTANCE hinstdll,
                          DWORD fdwReason,
                          LPVOID lpvReserved)
{
        return 1;
}

#define DLLEXPORT __declspec(dllexport)

#else
#define DLLEXPORT
#endif

#include <ctype.h>
#include <stdlib.h>
#include <math.h>


/* Test of constant input string */
DLLEXPORT
int
cs_count_upper (char* psz)
{
  int count = 0;

  if (psz) {
    while (*psz) {
      if (isupper (*psz))
        ++count;
      ++psz;
    }
    return count;
  } else
    return -1;
}

/* Test of input and output of a string */
DLLEXPORT
void
cs_to_upper (char* psz)
{
  if (psz) {
    while (*psz) {
      *psz = toupper (*psz);
      ++psz;
    }
  }
}

/* Test of an output only string */
DLLEXPORT
void
cs_make_random (int size, char* buffer)
{
  int i;
  for (i = 0; i < size; i++)
    buffer[i] = 'A' + (rand() % 26);
}


/* Test of input/output vector */
DLLEXPORT
void
half_double_vector (int size, double* vec)
{
  int i;
  for (i = 0; i < size; i++)
    vec[i] /= 2.;
}



