
%.o: %.f90
	$(FC) $(FFLAGS) -c $< 

%.mod: %.f90
	$(FC) $(FFLAGS) -c $< 

EXECS=bellhop tabssp

LIBSMODS=-L../misc -lmisc

all: $(EXECS)
	@echo "Bellhop built"
	@echo "*************"
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


# ******  bellhop ******

BELLHOPOBJS =	bellMod.o \
		angleMod.o \
                bdryMod.o  \
		sspMod.o \
		ArrMod.o \
		beampatternMod.o \
		readin.o  \
		spliner.o \
		munk.o

bellhop: bellhop.f90 $(BELLHOPOBJS)
	$(FC) $(FFLAGS) -o bellhop.exe bellhop.f90 $(BELLHOPOBJS) $(LIBSMODS)

readin.o:	readin.f90
	$(FC) $(FFLAGS) -c readin.f90

tabssp: tabssp.f90 munk.o
	$(FC) $(FFLAGS) -o tabssp.exe tabssp.f90 munk.o
