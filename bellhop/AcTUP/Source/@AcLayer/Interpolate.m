function InterpLayer = Interpolate(Layer1, Layer2, R1, R2, RInterp);
%This function horizontally interpolates between two definitions of the same layer corresponding to ranges R1 and R2 to produce a
%cell array of layers at ranges given in vector RInterp.
%
%The interpolation strategy is as follows:
%1. Thicknesses of layers are linearly interpolated.  This assumes all environments have the same number of layers.
% Where this isn't the case the interpolated environments will have a number of layers equal to the lesser of the number
% of layers in the two adjacent environments.  Layers are matched in order from the top layer down.  This means
% discontinuous intermediate layers aren't supported.
%
%2. Acoustic properties within a layer are linearly interpolated between the points on the bounding profiles that are closest
% in depth from the top of the layer.  The bounding profile with the greatest thickness is used to define the number
% of points (in depth) of the interpolated profiles.
%
%A J Duncan, 6/8/2004

NRange = length(RInterp);

%We want Layer1 to be the reference (thickest) layer - swap them over if this isn't the case
if Layer1.Z(end) < Layer2.Z(end)
    LTemp = Layer2;
    Layer2 = Layer1;
    Layer1 = LTemp;
    
    RTemp = R2;
    R2 = R1;
    R1 = RTemp;
end
    
RRef = [R1, R2];
NZ = length(Layer1.Z);
Cp = zeros(NZ, NRange);
Cs = zeros(NZ, NRange);
Rho = zeros(NZ, NRange);
Ap = zeros(NZ, NRange);
As = zeros(NZ, NRange);
    
RMSRough = interp1(RRef, [Layer1.RMSRough, Layer2.RMSRough], RInterp, 'linear');

for IZ1 = 1:NZ
    %Find the Layer 2 depth closest to each layer 1 depth
    Z1 = Layer1.Z(IZ1);
    [Val, IZ2] = min(abs(Layer2.Z - Z1));
    Z2 = Layer2.Z(IZ2);
    
    %Do the interpolation between these points
    Z(IZ1, :) = interp1(RRef, [Z1, Z2], RInterp, 'linear');
    Cp(IZ1, :) = interp1(RRef, [Layer1.Cp(IZ1), Layer2.Cp(IZ2)], RInterp, 'linear');
    Cs(IZ1, :) = interp1(RRef, [Layer1.Cs(IZ1), Layer2.Cs(IZ2)], RInterp, 'linear');
    Rho(IZ1, :) = interp1(RRef, [Layer1.Rho(IZ1), Layer2.Rho(IZ2)], RInterp, 'linear');
    Ap(IZ1, :) = interp1(RRef, [Layer1.Ap(IZ1), Layer2.Ap(IZ2)], RInterp, 'linear');
    As(IZ1, :) = interp1(RRef, [Layer1.As(IZ1), Layer2.As(IZ2)], RInterp, 'linear');
end

for IRange = 1:NRange
    %Build the cell array of layer objects
    P.Name = Layer1.Name;
    P.IsHalfSpace = Layer1.IsHalfSpace;
    P.RMSRough = RMSRough(IRange);
    
    P.Z = Z(:, IRange);
    P.Cp = Cp(:, IRange);
    P.Cs = Cs(:, IRange);
    P.Rho = Rho(:, IRange);
    P.Ap = Ap(:, IRange);
    P.As = As(:, IRange);
    
    InterpLayer{IRange} = AcLayer(P);
end
    
