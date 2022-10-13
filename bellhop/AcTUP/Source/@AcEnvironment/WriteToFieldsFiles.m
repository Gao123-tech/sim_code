function WriteToFieldsFiles(Env, fid, rmin, rmax, NdR, mode)
%WriteToScootFiles          Writes configuration file for Scooter to open target (ascii) file EnvFile
%
% USAGE                     WriteToFieldsFiles(fid, rmin, rmax, N)           no mode specified (DEFAULT)
%                           WriteToFieldsFiles(fid, rmin, rmax, N,  'N' )    'N'  mode (pass N)
%                           WriteToFieldsFiles(fid, rmin, rmax, dr, 'dr')    'dr' mode (calc N range slices from dr)
%
% INPUT    RunDef, Env, DirInfo   ->   See
%                          fid    =    writes to this open ascii file with this fid
%                         rmin    =    minimum range [m]
%                         rmax    =    maximum range [m]
%                            N    =    number of range slices OR
%                           dr    =    range increment
%                          mode   =    'DIRECT'   -> use RunDef.Fields.N [DEFAULT ... '', [], or omit: does the same thing]  
%                                      'CALC'     -> calculate N from RunDef.dR (as old WriteScooterFiles)
%
% Revision 0.0     13 July       2006  ... ALM
%                  >> SEPARATED From WriteScooterFiles (note plural)

if ~exist('mode', 'var'), mode = []  ;  end
if isempty(mode) || ~ischar(mode), 
   mode = 'N';
end

switch lower(mode) 
   case 'dr'
      dr = NdR;
      if rmax == rmin
         N = 1;
      else
         N = round((rmax - rmin)/dr) + 1;  %Number of ranges slices
      end
   otherwise  % N mode default !
      N  = NdR;
end
   
% Write output the required information to the .flp file
% OPTIONS: Cylindrical coords, linear output, integrate over +ve and -ve wavenumbers, polynomial interpolation
% (see fields.hlp for more info)
%     FieldOpt = '''RLBO''';
% RECENT CHANGES FOR AT 2006 Release by Mike Porter
%            See also AcToolboxFrontEnd->sGrn2Shd
FieldOpt = '''RBO''';
fprintf(fid, '%s\n', FieldOpt);
fprintf(fid, '%f  %f  %d\n', rmin/1000, rmax/1000, N);