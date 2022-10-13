function Str = GetLayerNames(Obj)
if isempty(Obj.LayerArr)
  	Str = '';
else
  	for ILayer = 1:length(Obj.LayerArr)
     	Str{ILayer} = GetName(Obj.LayerArr{ILayer});
  	end
end
