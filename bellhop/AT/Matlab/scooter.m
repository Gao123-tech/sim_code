function scooter( filename )
% useage: scooter( filename )
% where filename is the environmental file
%
% runs the SCOOTER program
% mbp Dec. 2002

if ( isempty( filename ) )
    warndlg( 'No envfil has been selected', 'Warning' );
else
    try copyfile( [ filename '.bty' ], 'BTYFIL' ); catch end
    try copyfile( [ filename '.trc' ], 'TRCFIL' ); catch end
    try copyfile( [ filename '.brc' ], 'BRCFIL' ); catch end
    try copyfile( [ filename '.irc' ], 'IRCFIL' ); catch end
    
    eval( [ '! scooter.exe < ' filename '.env > ' filename '.prt' ] );
    
    %! fields.exe < fields.flp
    %try movefile( 'SHDFIL', [ filename '.shd' ] ); catch end

    fieldsco( 'GRNFIL' );
    try movefile( 'SHDFIL.mat', [ filename '.mat' ] ); catch end
    
    warning off;
    delete 'BTYFIL';
    delete 'TRCFIL';
    delete 'BRCFIL';
    delete 'IRCFIL';
    warning on;
    
    try movefile( 'GRNFIL', [ filename '.grn' ] ); catch end
    
end

