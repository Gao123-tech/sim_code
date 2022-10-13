function krakenc( filename )
% useage: krakenc( filename )
% where filename is the environmental file
%
% runs the KRAKENC program
% mbp Dec. 2002

if ( isempty( filename ) )
    warndlg( 'No envfil has been selected', 'Warning' );
else
    try copyfile( [ filename '.bty' ], 'BTYFIL' ); catch end
    try copyfile( [ filename '.trc' ], 'TRCFIL' ); catch end
    try copyfile( [ filename '.brc' ], 'BRCFIL' ); catch end
    try copyfile( [ filename '.irc' ], 'IRCFIL' ); catch end
    
    eval( [ '! krakenc.exe < ' filename '.env > ' filename '.prt' ] );
    ! field.exe < field.flp
    
    warning off;
    delete 'BTYFIL';
    delete 'TRCFIL';
    delete 'BRCFIL';
    delete 'IRCFIL';
    warning on;
    
    try movefile( 'MODFIL0001', [ filename '.mod' ] ); catch end
    try movefile( 'SHDFIL',     [ filename '.shd' ] ); catch end
    
end

