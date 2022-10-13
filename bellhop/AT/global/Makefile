FC  = g95
FFLAGS= -c -Wunused-vars -Wunset-vars -std=f95 -O3 -fmod=../module -I../module


%.o: %.f90
	$(FC) $(FFLAGS) -c $< 

EXECS=toasc tobin tosac tonrl pod radius switchsr planewav

LIBSMODS=-L../lib -lmisc

all:	$(EXECS)
	@echo "Global tools built"
	@echo "******************"
	@echo " "

clean : clean_execs
	-rm -f *.o

clean_execs : 
	for f in $(EXECS) ; do \
	  if test -e $$f.exe ; then \
		(echo "Removing $$f" ; rm $$f.exe) ; \
	  fi ;  \
	done

#ph		echo "Installing $$f.exe"; cp -p $$f.exe $(AT)/bin;
install: all
	for f in $(EXECS) ; do \
	    ( if test -e $$f.exe ; then \
		echo "Installing $$f.exe"; cp -p $$f.exe ../bin; \
	    else \
		echo "$$f.exe not found"; \
	    fi ; ) \
	done

toasc:	toasc.f90 
	$(FC) $(FFLAGS) -o  toasc.exe toasc.f90 $(LIBSMODS)

tobin:	tobin.f90 
	$(FC) $(FFLAGS) -o  tobin.exe tobin.f90 $(LIBSMODS)

tosac:	tosac.f90 
	$(FC) $(FFLAGS) -o  tosac.exe tosac.f90 $(LIBSMODS)

tonrl:	tonrl.f90
	$(FC) $(FFLAGS) -o  tonrl.exe tonrl.f90 $(LIBSMODS)

pod:	pod.f90 transit.o
	$(FC) $(FFLAGS) -o  pod.exe pod.f90 transit.o $(LIBSMODS)

radius:	radius.f90 transit.o
	$(FC) $(FFLAGS) -o  radius.exe radius.f90 transit.o $(LIBSMODS)

bart:	bart.f90 
	$(FC) $(FFLAGS) -o  bart.exe bart.f90 $(LIBSMODS)

capon:	capon.f90 cholskic.o 
	$(FC) $(FFLAGS) -o  capon.exe capon.f90 cholskic.o $(LIBSMODS)

switchsr:	switchsr.f90 
	$(FC) $(FFLAGS) -o  switchsr.exe switchsr.f90 $(LIBSMODS)

planewav:	planewav.f90 
	$(FC) $(FFLAGS) -o  planewav.exe planewav.f90 $(LIBSMODS)

outback:	outback.f90 
	$(FC) $(FFLAGS) -o  outback.exe outback.f90 $(LIBSMODS)

