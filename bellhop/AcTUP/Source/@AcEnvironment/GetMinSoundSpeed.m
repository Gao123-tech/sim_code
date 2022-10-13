function CpMin = GetMinSoundSpeed(Env)
%Returns the minimum compressional sound speed for the Environment

if isempty(Env.LayerArr)
   CpMin = [];
else
   NLayer = length(Env.LayerArr);
	CpMin = GetMinSoundSpeed(Env.LayerArr{1});
	for ILayer = 2:NLayer
   	CpTemp = GetMinSoundSpeed(Env.LayerArr{ILayer});
      if CpTemp < CpMin
         CpMin = CpTemp;
      end
   end
end
