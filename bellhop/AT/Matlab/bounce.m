function bounce( filename )
% useage: bounce( filename )
% where filename is the environmental file
%
% runs the BOUNCE program
% mbp Dec. 2002

if ( isempty( filename ) )
    warndlg( 'No envfil has been selected', 'Warning' );
else
    %if ( exist( [ filename '.brc' ] ) == 2 );
    %   warndlg( 'Output Bottom Refl. Coef. file (BRCFIL) already exists', 'Warning' )
    %end
    
    eval( [ '! bounce.exe < ' filename '.env > ' filename '.prt' ] );
    
    if ( exist( 'BRCFIL' ) == 2 ); movefile( 'BRCFIL', [ filename '.brc' ] ); end   
    if ( exist( 'IRCFIL' ) == 2 ); movefile( 'IRCFIL', [ filename '.irc' ] ); end   
end

