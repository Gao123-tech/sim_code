
FC  = g95
FFLAGS= -c -Wunused-vars -Wunset-vars -std=f95 -O3 -fmod=$(AT)/module -I$(AT)/module

%.o: %.f90
	$(FC) $(FFLAGS) $< 

all:	munk.o readin.o RefCoMod.o SdRdRMod.o twersk.o \
	WriteHeader.o ReadHeader.o smooth.o subtab.o sorti.o \
	mergev.o weight.o splinec.o cpu_time.o polyz.o xerror.o \
	errout.o libmisc.a

clean:
	-rm -f libmisc.a
	-rm -f *.o
	-rm -f *.mod
	-rm -f *.lib
	-rm -f *.lst
	-rm -f *.BAK
	-rm -f *.fwd
# *** General purpose subroutines ***
# note: cpu_time is supposed to be a built-in f95 routine but is not in all compilers
# if your's has it then you can remove this version of cpu_time ...

SdRdRMod.mod:	SdRdRMod.f90
	$(FC) $(FFLAGS) SdRdRMod.f90

RefCoMod.mod:	RefCoMod.f90
	$(FC) $(FFLAGS) RefCoMod.f90


libmisc.a:	munk.o \
	readin.o \
	RefCoMod.o \
	SdRdRMod.o  \
	twersk.o \
	WriteHeader.o \
	ReadHeader.o \
	smooth.o \
	subtab.o \
	sorti.o \
	mergev.o \
	weight.o \
	splinec.o \
        polyz.o \
	xerror.o \
	errout.o \
	cpu_time.o
	ar -r libmisc.a errout.o WriteHeader.o mergev.o munk.o polyz.o \
		ReadHeader.o readin.o RefCoMod.o smooth.o sorti.o splinec.o \
		SdRdRMod.o subtab.o cpu_time.o twersk.o weight.o
	ranlib libmisc.a


	@echo "Routines needed by SCOOTER\SPARC\BELLHOP\BOUNCE built"
