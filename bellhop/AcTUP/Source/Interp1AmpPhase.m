function zi = Interp1AmpPhase(x, z, xi, method, extrap)
% InterpAmpPhase   Interpolates complex numbers at xi from pairs {(x,z)} by interpolating amplitude and phase 
%                  components seperately 
%
% USAGE:           zi = InterpAmpPhase(x, z, xi, method, extrap)     
%
%                  see interp1.m ... identical usage except x must be defined 
%                  default values are 
%                                     method = 'linear' 
%                                     extrap = 0  
%
% Revision History
% ================
% Revision 0.0    12 February 2003 ... ALM  
%                 - based entirely on AJD's 1d interp in AcT code: GetTFerFn.m
%
% -----------------------------------------------------------------------------------------------------------------------

if ~exists('method', var);
   method = 'linear';
end
if ~exists('extrap', var);
   extrap = 0;
end

% repeat interp1 for both bits - inefficient (esp for linear where its easy to write own interp routine one) 
% but slower only a factor of 2 in it at worst.

mag   = interp1(x, abs(zi),           xi, method, extrap);
phase = interp1(x, unwrap(angle(zi)), xi, method, extrap);

zi    = mag .* exp(i.*phase);