
%.o: %.f90
	$(FC) $(FFLAGS) -c $< 

%.mod: %.f90
	$(FC) $(FFLAGS) -c $< 

EXECS=scooter sparc fields 

LIBSMODS= -L../tslib  -L../misc  -lts -lmisc

all:	$(EXECS)
	@echo "Scooter and Sparc built"
	@echo "***********************"
	@echo " "

clean : clean_execs
	-rm -f *.o

clean_execs : 
	for f in $(EXECS) ; do \
	  if test -e $$f.exe ; then \
		(echo "Removing $$f" ; rm $$f.exe) ; \
	  fi ;  \
	done

install: all
	for f in $(EXECS) ; do \
	    ( if test -e $$f.exe ; then \
		echo "Installing $$f.exe"; cp -p $$f.exe ../bin; \
	    else \
		echo "$$f.exe not found"; \
	    fi ; ) \
	done

# ******  scooter ******
SCOOTEROBJS = scomod.o facnpZ.o ../misc/libmisc.a
scooter: scooter.f90 $(SCOOTEROBJS)
	$(FC) $(FFLAGS) -o scooter.exe scooter.f90 $(SCOOTEROBJS) $(LIBSMODS)

#***** sparc *****
SPARCOBJS=spamod.o facnp.o ../tslib/libts.a
sparc:	sparc.f90 $(SPARCOBJS)
	$(FC) $(FFLAGS) -o sparc.exe sparc.f90 $(SPARCOBJS) $(LIBSMODS)

# ****** fields ******
FIELDSOBJS=hts.o pade.o polyc.o
fields:	fields.f90 $(FIELDSOBJS)
	$(FC) $(FFLAGS) -o fields.exe fields.f90 $(FIELDSOBJS) $(LIBSMODS)

# ****** plotts ******
plotts:	plotts.f90
	$(FC) $(FFLAGS) -o plotts.exe plotts.f90 $(LIBSMODS)

