function CsMin = GetMinShearSpeed(Env)
%Returns the minimum shear speed for the Environment
%Returns [] if all layers are fluid (Cs = 0)

CsMin = [];
if ~isempty(Env.LayerArr)
   NLayer = length(Env.LayerArr);
	for ILayer = 1:NLayer
   	CsTemp = GetMinShearSpeed(Env.LayerArr{ILayer});
      if ~isempty(CsTemp) & (isempty(CsMin) | (CsTemp < CsMin))
         CsMin = CsTemp;
      end
   end
end
