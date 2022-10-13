function RunDef = UpdateRunDefinition(RunDef)
%UpdateRunDefinition    calculates parameters not directly specified or sorts out inconsistent data
%
%                RunDef = UpdateRunDefinition(RunDef)  ;
%
% Revision 0.0   01 December 2004 ... ALM
%                - at this stage all that is required is that we calculate range increment  
%                - and there's some legacy code to cope with

if nargin ~= 1
   error('ERROR -> UpdateRunDefinition: Incorrect number of parameters');
else
   if RunDef.NRange <= 1
      RunDef.dR = 0;
   else
      RunDef.dR = (RunDef.RMax - RunDef.RMin) / (RunDef.NRange - 1);
      % this is necessary to run existing functions for bellhop when the "extra parameters" are wholey extracted
      RunDef.Bellhop.UseBathyFile = RunDef.UseBathFile ;
      RunDef.Bellhop.BathyFName   = RunDef.BathFName   ;
   end
end
