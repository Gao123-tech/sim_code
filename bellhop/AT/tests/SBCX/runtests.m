% SBCX test cases
% These tests exercise the arrivals option in Bellhop
% verifying that the results obtained by summing from the arrivals file
% match those when BELLHOP directly outputs a pressure field.
%
% mbp

bellhop( 'sbcx' )
plotshd( [ 'sbcx.shd' ], 3, 1, 1 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

bellhop( 'sbcx_Arr_asc' )
makeshdarr( 'sbcx_Arr_asc', 200, 'ascii' )
plotshd( 'sbcx_Arr_asc.mat', 3, 1, 2 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

bellhop( 'sbcx_Arr_bin' )
makeshdarr( 'sbcx_Arr_bin', 200, 'binary' )
plotshd( 'sbcx_Arr_bin.mat', 3, 1, 3 )
caxis( [ 40 80 ] ); colorbar( 'horiz' )

% use following for Matlab Bellhop which produces a *.mat file
%bellhopM( 'sbcx' )
%plotshd( [ 'sbcx.mat' ], 3, 1, 1 )
%caxis( [ 40 80 ] ); colorbar( 'horiz' )

%bellhopM( 'sbcx_Arr_asc' )
%makeshdarr( 'sbcx_Arr_asc', 200, 'matlab' )
%plotshd( 'sbcx_Arr_asc.mat', 3, 1, 3 )
%caxis( [ 40 80 ] ); colorbar( 'horiz' )
