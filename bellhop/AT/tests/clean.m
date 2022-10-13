% clean
% deletes all temporary files

delete *.prt
delete *.shd
delete *.grn
delete *.mod
delete *.mat
delete *.arr
delete *.asv
delete *.rts

cases = [ 'free       '; ...
          'halfspace  '; ...
          'calib      '; ...
          'Munk       '; ...
          'sduct      '; ...
          'Dickins    '; ...
          'arctic     '; ...
          'SBCX       '; ...
          'BeamPattern'; ...
          'TabRefCoef '; ...
          'PointLine  '; ...
          'ParaBot    '; ...
          'head       ';];

for icase = 1: size( cases, 1 )
    directory = deblank( cases( icase, : ) )
    eval( [ 'cd ' directory ] );
    delete *.prt
    delete *.shd
    delete *.grn
    delete *.mod
    delete *.mat
    delete *.arr
    delete *.asv
    delete *.rts
    delete *.ray
    delete *.irc
    delete *.brc
    delete GRNFIL
    delete SHDFIL
    cd ..
end

