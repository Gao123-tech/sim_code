function CsMin = GetMinShearSpeed(Layer)
%Returns the minimum shear speed for the layer
%Returns [] if the layer is a fluid (Cs = 0)

CsMin = min(Layer.Cs);
if CsMin == 0
   CsMin = [];
end
