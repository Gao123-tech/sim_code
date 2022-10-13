function CpMax = GetMaxSoundSpeed(Env)
%Returns the maximum compressional sound speed for the Environment

if isempty(Env.LayerArr)
   CpMax = [];
else
   NLayer = length(Env.LayerArr);
	CpMax = GetMaxSoundSpeed(Env.LayerArr{1});
	for ILayer = 2:NLayer
   	CpTemp = GetMaxSoundSpeed(Env.LayerArr{ILayer});
      if CpTemp > CpMax
         CpMax = CpTemp;
      end
   end
end
