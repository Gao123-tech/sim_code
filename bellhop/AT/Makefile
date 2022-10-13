# 4/28/2005
# Paul Hursky (revisions, MBP)
#
# first, delete all the binary files that came with the $AT package
# make clean
#
# Then, rebuild $AT
# make all
#
# Change this line so that AT is defined to be the top level
# Acoustics Toolbox directory (i.e. the directory this file
# should be located in)

export AT="d:/Program Files/CMST Software/AcTUP v2.2L/AT"

# make sure your g95 executable is in your path or use
# the full pathname for g95 in the next line. 

export FC=g95
export FFLAGS= -Wunused-vars -Wunset-vars -std=f95 -O3 -fmod=$(AT)/module -I$(AT)/module
#export FFLAGS= -Wunused-vars -Wunset-vars -std=f95 -O3 -ftrace=full -fbounds-check -fmod=$(AT)/module -I$(AT)/module

export RM=rm
export CC=gcc
export CFLAGS=-g

all:
	(cd misc;	make -k all)
	(cd tslib;	make -k all)
	(cd global;	make -k all)
	(cd Kraken;	make -k all)
	(cd Scooter;	make -k all)
	(cd Bellhop;	make -k all)
	@echo " "
	@echo "*************************************"
	@echo "****** Acoustics Toolbox built ******"
	@echo "*************************************"

install:
	(cd tslib;	make -k install)
	(cd global;	make -k install)
	(cd Kraken;	make -k install)
	(cd Scooter;	make -k install)
	(cd Bellhop;	make -k install)
	@echo " "
	@echo "*************************************"
	@echo "****** Acoustics Toolbox installed **"
	@echo "*************************************"

clean:
	(rm -f */*.o lib/*.a module/*.mod)
	(cd misc;	make -k -i clean)
	(cd tslib;	make -k -i clean)
	(cd global;	make -k -i clean)
	(cd Kraken;	make -k -i clean)
	(cd Scooter;	make -k -i clean)
	(cd Bellhop;	make -k -i clean)
	(cd tests;	make -k -i clean)

