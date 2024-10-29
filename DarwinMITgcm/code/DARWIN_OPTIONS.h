#ifndef DARWIN_OPTIONS_H
#define DARWIN_OPTIONS_H
#include "PACKAGES_CONFIG.h"
#ifdef ALLOW_DARWIN

#include "CPP_OPTIONS.h"

CBOP
C    !ROUTINE: DARWIN_OPTIONS.h
C    !INTERFACE:

C    !DESCRIPTION:
C options for darwin package
CEOP

C tracer selection

#undef  DARWIN_ALLOW_NQUOTA
#undef  DARWIN_ALLOW_PQUOTA
#undef  DARWIN_ALLOW_FEQUOTA
#undef  DARWIN_ALLOW_SIQUOTA
#define DARWIN_ALLOW_CHLQUOTA
#undef  DARWIN_ALLOW_CDOM
#define DARWIN_ALLOW_CARBON

C optional bits

#undef  DARWIN_ALLOW_DENIT
#undef  DARWIN_ALLOW_EXUDE
#define ALLOW_OLD_VIRTUALFLUX

C light

#undef  DARWIN_AVPAR
#define DARWIN_ALLOW_GEIDER

C initialize chl with radtrans as in darwin2
#undef  DARWIN_CHL_INIT_LEGACY

#define DARWIN_GEIDER_RHO_SYNTH

C grazing

C for quadratic grazing a la quota
#undef  DARWIN_GRAZING_SWITCH

C compute palat from size ratios
#undef  DARWIN_ALLOMETRIC_PALAT

C turn off grazing temperature dependence
#undef  DARWIN_NOZOOTEMP

#undef  DARWIN_TIME_GRAZING

C temperature

#undef  DARWIN_NOTEMP
#define DARWIN_TEMP_VERSION 2
#define  DARWIN_TEMP_RANGE

C iron

#define DARWIN_MINFE
#define DARWIN_PART_SCAV
#define DARWIN_IRON_SED_SOURCE_VARIABLE

C debugging

#undef  DARWIN_DEBUG

#define DARWIN_ALLOW_CONS

#define DARWIN_UNUSED 0

C deprecated

C these are for darwin_generate_random
#undef  DARWIN_RANDOM_TRAITS
#undef  DARWIN_TWO_SPECIES_SETUP
#undef  DARWIN_NINE_SPECIES_SETUP
#undef  DARWIN_ALLOW_DIAZ

#endif /* ALLOW_DARWIN */
#endif /* DARWIN_OPTIONS_H */

