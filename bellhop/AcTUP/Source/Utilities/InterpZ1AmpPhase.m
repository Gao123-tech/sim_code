function zi = InterpZ1AmpPhase(x, z, xi, imode, emode)
% InterpAmpPhase   Interpolates complex numbers at xi from pairs {(x,z)} by interpolating amplitude and phase 
%                  components seperately 
%
% USAGE:           zi = InterpZ1AmpPhase(x, z, xi, imode, emode)     
%
%                  see interp1.m ... identical usage except x must be defined 
%                  default values are 
%                                     method = 'linear' 
%                                     extrap = 0 
%
%                   NOTE: imode is one of the interpolation modes offered by interp1
%                         emode can by either 
%                               i) the string 'extrap' which applies the same method as imode to extrapolation OR
%                              ii) a value which is forced on all xi outside x-domain
%                          *** This is identical to interp1 although badly annotated in matlab help ***
%                              (i.e. "specified method doesnt mention interploation method AND
%                                    the string variable 'method' and constant 'extrap' both have quotes around them!!)                
%
%
% Amos L Maggi
% Centre for Marine Sciecne and Technology
% Department of Applied Physics 
% Curtin University
%
% Revision History
% ================
% Revision 0.0    18 February 2003 ... ALM  
%                 - based on AJD's 1d interp in AcT code: GetTFerFn.m with additional flexibility written in
%
% -----------------------------------------------------------------------------------------------------------------------

if ~exist('imode', 'var');
   method = 'linear';
end
if ~exist('emode', 'var');
   extrap = 0;
else
   if ischar(emode) 
      if ~strcmpi(emode, 'extrap')
         disp('ERROR:InterpZ1AmpPhase -> only valid input for emode is the string ''extrap'' or a (complex) value');
         return;
      end
   end
end

% repeat interp1 for both bits - inefficient (esp for linear where its easy to write own interp routine one) 
% but slower only a factor of 2 in it at worst.

mag   = interp1(x, abs(z),           xi, imode, emode);
phase = interp1(x, unwrap(angle(z)), xi, imode, emode);

zi    = mag .* exp(i.*phase);