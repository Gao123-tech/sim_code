function  DefOut = SetFieldsParams(DefIn)
%SetFieldsParams    Edits Fields-Specific Propagation parameters of Run Definition  ------------------
%
% USAGE:        Def          = SetFieldsParams(Def)
%
%                              If there is a problem Def will remain unmodified
%
% See AcTUP for object definitions
%
% Revision History
% ================
% Revision 0.0      xx ???????   ???? ... AJD
%                   - Originally incorporated in AcToolboxFrontEnd.m as lSetFieldsParams
% Revision 1.0      07 July      2006 ... ALM
%                   - Parsed into function
%                   - Status removed as primary return variable - in fact not sure it is even needed
% --------------------------------------------------------------------------------------------------------


Def    = DefIn;
DefOut = DefIn;

% get grn filename
[fname, pname] = uigetfile('*.grn', 'FIELDS - Select representative Green''s function file (template)');
if fname == 0
   return;
end

%get frequency and base filename 
[f, bname] = Filename2Params(StripExtension([pname fname], 'p'),'');

Def.Fields.GrnFilename = fname    ;
Def.Fields.GrnPathname = pname    ;
Def.Fields.GrnBasename = bname    ;

% read grn file
[zsvec, zrvec, f, Kr, GKr, Title] = ReadGreen([pname fname]);

% wavenumber vector detail
dk   = Kr(2)-Kr(1);
kmax = Kr(end);

% Rmax(dk): the equation below 
% Rmax_dk = 2*pi / dk                ; % is correct however ...
% Alec noticed that numerical problems resulted from 
% integration to green's fn (suspects caused by contour avoiding x-axis poles) - he solved this by 
% nominally using rmax_scooter = 2*rmax_user - 
% the problems seemed to be resticted between rmax_scooter and rmax_scooter/2 = rmax_user
% 
Rmax_dk  = pi / dk                  ; 
% we maintain this even though the problem may have been solved in AT2006 - need to be  checked!!

% dR(k): if this is a nyquist condition as expected then it should be pi not 2*pi ...
% BUT if Kr(1) is -knyq [not dk or zero] and Kr(end) + knyq then we need the 2 !
% i.e. cetainly the following equation appears in FIELDS suggesting both halves of the k-spec are used 
dR_k    = 2*pi / (Kr(end)-Kr(1)) ;

% extract data from main definition and give user opportunity to switch
tstr = 'TL Range Grid ~ Guide Only -> FIELDS may increase rmax (reduce dk & interpolate S(k)) &/or reduce dr (set S(k)=0: k > kmax)';
done = 0;
while ~done
   defans_num = [Def.Fields.rmin, Def.Fields.rmax, Def.Fields.nr ];
   if defans_num(2) == 0, defans_num(2) = Def.RMax   ; end
   if defans_num(3) == 0, defans_num(3) = Def.NRange ; end 
   for ii=1:3, defans{ii} = num2str(defans_num(ii)); end
   %
   %note - the spacings in the strings below are uneven but tuned for sanserif 10point on display - don't mess with it
   Ans = inputdlg({['r-min for TL calc        < rmin(ip) = ', num2str(Def.RMin   ) , 'm >' ] ,...
                   ['r-max for TL calc       < rmax(ip) = ' , num2str(Def.RMax   ) , 'm  '   ,...
                                             'rmax(dk) = '  , num2str(Rmax_dk    ) , 'm >' ] ,...
                   ['number of slices       < nr(in) = '    , num2str(Def.NRange ) , '   '   ,...
                                             'dr(in) = '    , num2str(Def.dR     ) , 'm  '   ,...
                                             'dr(k)  = '    , num2str(    dR_k   ) , 'm >' ] ,...
                  }, tstr, [1,150], defans);
   if isempty(Ans)
      % CANCELLED
      done   = 1;      
   else
      Def.Fields.rmin = str2num(Ans{1});
      Def.Fields.rmax = str2num(Ans{2});
      Def.Fields.nr   = str2num(Ans{3});
      defans = Ans;
      if Def.Fields.rmax <= 0 ||  Def.Fields.nr <= 0          
         waitfor(errordlg('Maximum range and number of slices must both be > 0','TL Range Grid IP ERROR','modal'));
      elseif Def.Fields.rmax < Def.Fields.rmin || Def.Fields.rmin < 0
         waitfor(errordlg('Minimum range must be: 0 <= rmin <= rmax','TL Range Grid IP ERROR','modal'));
      else         
         % SUCCESS
         done = 1;
         DefOut = Def;
      end
   end
end
