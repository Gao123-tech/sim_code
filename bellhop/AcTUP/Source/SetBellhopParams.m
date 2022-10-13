function  [Def, Status] = SetBellhopParams(Def)
%SetBellhopParams    Edits Bellhop-Specific Propagation parameters of Run Definition  ------------------
%
% USAGE:        Def          = SetBellhopParams(Def)
%  or          [Def, Status] = SetBellhopParams(Def)
%
%              Status        = 0 if operation cancelled by user
%                            = 1 otherwise
%
% See AcTUP for object definitions
%
% Revision History
% ================
% Revision 0.0      xx ???????   ???? ... AJD
%                   - Originally incorporated in AcToolboxFrontEnd.m as lSetBellhopParams
% Revision 0.1      18 March 2005     ... ALM
%                   - add beam desplacement switch ... in BELLHOP this is BeamType(3:3)
%                     this assigned in READIN afer RunType read as -> BeamType(3:3) = RunType(3:3)
% Revision 1.0      06 July      2006 ... ALM
%                   - Parsed into function
%                   - Status removed as primary return variable - in fact not sure it is even needed
% --------------------------------------------------------------------------------------------------------

% INITIALISE STUFF ________________________________________________________________________
FALSE = 0;
TRUE  = 1;

KnownRunTypes  = {'R', 'A', 'C', 'I', 'S'};
KnownBeamTypes = {'B', 'G'};

%Warn user if any of these combinations of run and beam  types are specified
BadCombinations   = {'AS', 'aS', 'IS', 'SS' };
SuggestedBeamType = {'G' , 'G' , 'G' , 'G'  };

DirInfo = GetAcDirectoryInfo;

% INPUT STUFF  ________________________________________________________________________
Prompt = {...
   'Run type: R = ray, C = coherent TL, I = incoherent TL, S = semicoherent TL,  A = amplitude-delay', ...
   'Beam type: B = Gaussian bundles,  G = geometric', ...   
   'Beam displacement: 0/1 = off/on', ...
   'Number of beams', ...
   'Launch angle of 1st ray (degrees from horizontal, -ve towards surface)', ...
   'Launch angle of last ray (degrees from horizontal, -ve towards surface)', ...
   'Step size along ray for raytrace (wavelengths, 0 to let Bellhop choose)'};

Done  = 0; 
while ~Done
   Opt1 = Def.Bellhop.RunType(1);
   if length(Def.Bellhop.RunType) >= 2
      Opt2 = Def.Bellhop.RunType(2);
      if length(Def.Bellhop.RunType) >= 3
         if Def.Bellhop.RunType(3) == 'S'
            BeamDisp = TRUE;
         else
            BeamDisp = FALSE;
         end
      else
         BeamDisp = FALSE;
      end
   else
      Opt2 = '';
   end

   DefAns = {Opt1, Opt2, num2str(BeamDisp), num2str(Def.Bellhop.NBeams), num2str(Def.Bellhop.StartAngle), ...
      num2str(Def.Bellhop.EndAngle),  num2str(Def.Bellhop.StepSize)};


   Ans = inputdlg(Prompt,'Bellhop specific parameters', [1,100], DefAns);

   if isempty(Ans)
      Status = 0;
      Done = 1;
   else
      Status = 1;
      if str2num(Ans{3})
         RunType3 = 'S';
      else
         RunType3 = '' ;
      end         
      Def.Bellhop.RunType = [Ans{1} Ans{2} RunType3];
      Def.Bellhop.NBeams = str2num(Ans{4});
      Def.Bellhop.StartAngle = str2num(Ans{5});
      Def.Bellhop.EndAngle = str2num(Ans{6});
      Def.Bellhop.StepSize  = str2num(Ans{7});
   end
   
   if ~Status, return; end;
   
   Def = GetBathymetryFilename(Def, DirInfo);

   %Check the parameters are OK _______________________________________________________________
   FoundErr = 0;
   RT = Def.Bellhop.RunType;
   IType = find(strcmp(RT(1), KnownRunTypes));
   if isempty(IType)
      Msg = ['Unknown run type: ' RT(1)];
      FoundErr = 1;
   end

   if ~FoundErr
      IType = find(strcmp(RT(2), KnownBeamTypes));
      if isempty(IType)
         Msg = ['Unknown beam type: ' RT(2)];
         FoundErr = 1;
      end
   end

   if ~FoundErr
      IBad = find(strcmp(Def.Bellhop.RunType, BadCombinations));
      if ~isempty(IBad)
         Msg = {['Bellhop does not currently implement run type: ' RT(1) ' with beam type: ' RT(2)], ...
            ['Try beam type: ' SuggestedBeamType{IBad(1)} ' instead']};
         FoundErr = 1;
      end
   end

   if FoundErr
      uiwait(warndlg(Msg));
      switch(menu('', 'Modify', 'Continue anyway'));
         case 2,
            Done = 1;
      end
   else
      Done = 1;
   end
end

