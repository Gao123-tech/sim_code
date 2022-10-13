function IsPek = IsPekeris(Obj)
%Checks whether the environment object is a Pekeris environment
%(Single sound speed water column over an acoustic half-space)

N = length(Obj.LayerArr);
IsPek = (N == 2) & IsPekerisWaterLayer(Obj.LayerArr{1}) & IsPekerisBottom(Obj.LayerArr{2})
