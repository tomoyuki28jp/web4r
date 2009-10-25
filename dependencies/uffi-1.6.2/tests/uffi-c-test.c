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


DLLEXPORT unsigned char uchar_13 = 13;
DLLEXPORT signed char schar_neg_120 = -120;
DLLEXPORT unsigned short uword_257 = 257;
DLLEXPORT signed short sword_neg_321 = -321;
DLLEXPORT unsigned int uint_1234567 = 1234567;
DLLEXPORT signed int sint_neg_123456 = -123456;
DLLEXPORT double double_3_1 = 3.1;
DLLEXPORT float float_neg_4_5 = -4.5;

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



DLLEXPORT
void *
cast_test_int () {
  int *x = (int *) malloc(sizeof(int));
  *x = 23;
  return x;
}

DLLEXPORT
void *
cast_test_float ()
{
  double *y = (double *) malloc(sizeof(double));
  *y = 3.21;
  return y;
}

DLLEXPORT
long
return_long_negative_one ()
{
  return -1;
}

DLLEXPORT
int
return_int_negative_one ()
{
  return -1;
}

DLLEXPORT
short
return_short_negative_one ()
{
  return -1;
}

DLLEXPORT int fvar_addend = 3;

typedef struct {
  int i;
  double d;
} fvar_struct_type;

fvar_struct_type fvar_struct = {42, 3.2};

DLLEXPORT
int fvar_struct_int () {
  return (fvar_addend + fvar_struct.i);
}

DLLEXPORT
double fvar_struct_double () {
  return fvar_struct.d;
}


