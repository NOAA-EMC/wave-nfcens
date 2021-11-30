########################################################
PROGRAM =   nfcombwave_ensemble
########################################################
SHELL=/bin/sh
#
CC       =
CFLAGS   =
CPPFLAGS =
FC =            ftn
FFLAGS =        -O
LDFLAGS = 
LIBS =          $(G2_LIB4) $(W3NCO_LIB4) $(BACIO_LIB4) $(JASPER_LIB) $(PNG_LIB) $(Z_LIB)

SRCS=	grbit2.f90  nfcombwave_ensemble.f90
OBJS     = $(addsuffix .o, $(basename $(SRCS)))



# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
default:		$(PROGRAM)

$(PROGRAM):		$(OBJS)
		$(FC) $(LDFLAGS) -o $(PROGRAM) $(OBJS) $(LIBS)
clean :
	\rm -f *.o *.mod

########################################################
# compile rules
########################################################
.SUFFIXES : .f .f90 .F .F90 .c

%.o : %.c
	$(CC) -c $(CFLAGS) $<

%.o : %.f90
	$(FC) -c $(FFLAGS) $<

%.o : %.F90
	$(FC) -c $(FFLAGS) $<

%.o : %.f
	$(FC) -c $(FFLAGS) $<

%.o : %.F
	$(FC) -c $(FFLAGS) $<

########################################################
# dependencies
########################################################

grbit2.o:   grbit2.f90 

nfcombwave_ensemble.o:	nfcombwave_ensemble.f90 grbit2.o

