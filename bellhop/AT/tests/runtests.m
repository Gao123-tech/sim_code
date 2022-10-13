% plottests
% runs a battery of test problems for the Acoustics Toolbox

clear all

% general tests

figure
plottri( 'lant' )

% SPARC
sparc( 'iso' )
plotts( 'iso.rts' )

% bounce
bounce( 'refl' )
plotrth( 'refl' )

cd free
runtests
cd ..

cd halfspace
runtests
cd ..

cd calib
runtests
cd ..

cd Munk
runtests
cd ..

cd sduct
runtests
cd ..

cd Dickins
runtests
cd ..

cd arctic
runtests
cd ..

cd SBCX
runtests
cd ..

cd BeamPattern
runtests
cd ..

cd TabRefCoef
runtests
cd ..

cd PointLine
runtests
cd ..

cd ParaBot
runtests
cd ..

cd head
runtests
cd ..

% TL slice tests from manual
runtests_tl
