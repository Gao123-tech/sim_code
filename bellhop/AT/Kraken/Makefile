
%.o: %.f90
	$(FC) $(FFLAGS) -c $< 

%.mod: %.f90
	$(FC) $(FFLAGS) -c $< 

EXECS=kraken krakenc bounce field field3d fieldmo modasc modbin
MODULES=krakmod.f90 krakcmod.f90
LIBSMODS=-L../misc -lmisc

all :	$(EXECS)
	@echo "KRAKEN built"
	@echo "*************"
	@echo " "
 
install : $(EXECS)
	for f in $(EXECS) ; do \
	    ( if test -e $$f.exe ; then \
		echo "Installing $$f.exe"; cp -p $$f.exe ../bin; \
	    else \
		echo "$$f.exe not found"; \
	    fi ; ) \
	done

clean : clean_execs
	-rm -f *.o

clean_execs : 
	for f in $(EXECS) ; do \
	  if test -e $$f.exe ; then \
		(echo "Removing $$f" ; rm $$f.exe) ; \
	  fi ;  \
	done

modules: $(MODULES)

# ****** kraken ******
KRAKENOBJS=zsecx.o zbrentx.o bcimp.o kuping.o pekrt.o sinvitd.o krakmod.o
kraken:	kraken.f90 $(KRAKENOBJS) krakmod.o
	$(FC) $(FFLAGS) -o kraken.exe kraken.f90 $(KRAKENOBJS) $(LIBSMODS)

bcimp.o:	bcimp.f90 krakmod.o
	$(FC) $(FFLAGS) -c bcimp.f90

# ****** krakenc ******
KRAKENCOBJS=zseccx.o sinvitz.o bcimpc.o	kuping.o pekrt.o krakcmod.o
krakenc: krakenc.f90 $(KRAKENCOBJS) krakcmod.o
	$(FC) $(FFLAGS) -o krakenc.exe krakenc.f90 $(KRAKENCOBJS) $(LIBSMODS)
bcimpc.o: bcimpc.f90 krakcmod.o
	$(FC) $(FFLAGS) -c bcimpc.f90

# ****** bounce ******
BOUNCEOBJS=bcimpc.o kuping.o pekrt.o krakcmod.o
bounce:	bounce.f90 $(BOUNCEOBJS)
	$(FC) $(FFLAGS) -o bounce.exe bounce.f90 $(BOUNCEOBJS) $(LIBSMODS)

# ****** field ******
FIELDOBJS=getmod.o pekrt.o eval.o evalad.o evalcm.o
field:	field.f90 $(FIELDOBJS)
	$(FC) $(FFLAGS) -o field.exe field.f90 $(FIELDOBJS) $(LIBSMODS)

# ****** field3d ******
FIELD3DOBJS=evalpdq.o eval3d.o evalgb.o getmod.o pekrt.o
field3d: field3d.f90 $(FIELD3DOBJS)
	$(FC) $(FFLAGS) -o field3d.exe field3d.f90 $(FIELD3DOBJS) $(LIBSMODS)

# ****** fieldmo ******
FIELDMOOBJS=getmod.o pekrt.o evalmo.o
fieldmo: fieldmo.f90 $(FIELDMOOBJS)
	$(FC) $(FFLAGS) -o fieldmo.exe fieldmo.f90 $(FIELDMOOBJS) $(LIBSMODS)

modasc: modasc.f90
	$(FC) $(FFLAGS) -o modasc.exe modasc.f90
modbin: modbin.f90
	$(FC) $(FFLAGS) -o modbin.exe modbin.f90

# ****** covar ******
COVAROBJS=getmod.o pekrt.o
covar:	covar.f90 $(COVAROBJS)
	$(FC) $(FFLAGS) -o covar.exe covar.f90 $(COVAROBJS) $(LIBSMODS)

