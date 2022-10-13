function SoundSpeed = GetBottomSoundSpeed(Env)
%This function obnly makes sense for a 2-layer environment.  Returns an empty matrix for any other environment.

SoundSpeed = [];

if length(Env.LayerArr) == 2 
     SoundSpeed = GetMaxSoundSpeed(Env.LayerArr{end});
end
