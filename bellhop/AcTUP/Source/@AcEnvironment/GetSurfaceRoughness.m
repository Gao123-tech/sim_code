function Rms = GetSurfaceRoughness(Env)
%Returns the RMS roughness of the water surface

Layer = GetWaterColumn(Env);
Rms = GetInterfaceRoughness(Layer);