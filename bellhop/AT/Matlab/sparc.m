function sparc( filename )
% useage: sparc( filename )
% where filename is the environmental file
%
% runs the SPARC program
% mbp Dec. 2002

if ( isempty( filename ) )
    warndlg( 'No envfil has been selected', 'Warning' );
else
    try copyfile( [ filename '.sts' ], 'STSFIL' ); catch end
 
    eval( [ '! sparc.exe < ' filename '.env > ' filename '.prt' ] );
 
    try movefile( 'GRNFIL', [ filename '.grn' ] ); catch end
    try movefile( 'RTSFIL', [ filename '.rts' ] ); catch end
    
end

