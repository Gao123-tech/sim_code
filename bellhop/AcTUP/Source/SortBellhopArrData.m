function ArrData = SortBellhopArrData(ArrData, mode)
%SortBellhopArrData       Sorts Bellhop Arrival File Data Structure - assumes only one SD, RD and RR value !
%                         ie ArrData.Amp -> ArrData.Amp(1,A,1,1) ... A = ArrData.NArrMat is a scalar
%
%USAGE                    ArrData = SortBellhopArrData(ArrData, mode)
%
%                         ArrData.Amp
%                                .Delay
%                                .SrcAngle
%                                .RcvrAngle
%                                .NumTopBnc
%                                .NumBotBnc
%                                .NArrMat
%                                .NSD
%                                .NRD
%                                .NR
%                                .SD
%                                .RD
%                                .RR
%
%                          mode              =  'DELAY'  sorts in increasing time  (or 'DELAY+') earliest arrivals first
%                                               'DELAY-' sorts in decreasing time  
%                                               'AMP'    sorts in decreasing amplitude (or 'AMP-')
%                                               'AMP+'   sorts in increasing amplitude 
%
%Revision 0.0     21 September 2005 ... Amos L Maggi
%                 - only time (delay) and amplitude sorts implemented at this stage
persistent pathname;

TRUE  = 1;
FALSE = 0;

if nargin < 2
   return;
elseif isempty(ArrData)
   return;
end

switch upper(mode)
   case {'DELAY', 'DELAY+'}
      %sort in increasing delay
      [temp, isort] = sort(ArrData.Delay(1,:,1,1), 'ascend' );
   case 'DELAY-'
      %sort in decreasing delay
      [temp, isort] = sort(ArrData.Delay(1,:,1,1), 'descend');
   case 'AMP+'
      %sort in increasing amplitude
      [temp, isort] = sort(ArrData.Amp(1,:,1,1)  , 'ascend' );
   case {'AMP', 'AMP-'}
      %sort in decreasing amplitude
      [temp, isort] = sort(ArrData.Amp(1,:,1,1)  , 'descend');
end

ArrData.Amp         = ArrData.Amp(1,isort,1,1)         ;
ArrData.Delay       = ArrData.Delay(1,isort,1,1)       ;
ArrData.SrcAngle    = ArrData.SrcAngle(1,isort,1,1)    ;
ArrData.RcvrAngle   = ArrData.RcvrAngle(1,isort,1,1)   ;
ArrData.NumTopBnc   = ArrData.NumTopBnc(1,isort,1,1)   ;
ArrData.NumBotBnc   = ArrData.NumBotBnc(1,isort,1,1)   ;


