function Const = GetPhysicalConstants(Env)

Water = GetWaterColumn(Env);

%The following aren't strictly correct as we should really give density and sound speed at the prop depth
Const.Rho0 = GetMaxDensity(Water);  %Density of sea water
Const.SoundSpeed = GetMaxSoundSpeed(Water);  %Sound speed in water (m/s)
