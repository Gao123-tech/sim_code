MODULE sspmod

   ! holds SSP input by user and associated variables

   IMPLICIT NONE
   SAVE
   INTEGER MXSSP
   PARAMETER ( MXSSP = 2001 )

   INTEGER NSSP, LAYER
   REAL ( KIND = 4 ) :: ZSSPV( MXSSP ), CSSPV( MXSSP ), CZV( MXSSP ), N2V( MXSSP ), N2ZV( MXSSP ), CVS( 4, MXSSP )
END MODULE sspmod
