% plottests
% runs a battery of test problems for the Acoustics Toolbox

clear all

% general tests

figure
plottri( 'lant' )

% SPARC
sparcM( 'iso' )
plottsmat iso

% bounce
bounce( 'refl' )
plotrth( 'refl' )

cd halfspace
runtestsM
cd ..

cd calib
runtestsM
cd ..

cd Munk
runtestsM
cd ..

cd sduct
runtestsM
cd ..

cd Dickins
runtestsM
cd ..

cd arctic
runtestsM
cd ..

cd SBCX
%runtestsM
cd ..

cd BeamPattern
runtestsM
cd ..

cd TabRefCoef
runtestsM
cd ..

cd PointLine
%runtestsM
cd ..

cd head
%runtests
cd ..

% TL slice tests from manual
runtests_tlM
