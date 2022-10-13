function SoundSpeed = GetBottomSoundSpeed(Layer)
%Returns the sound speed at the bottom of the specified layer

SoundSpeed = Layer.Cp(end);