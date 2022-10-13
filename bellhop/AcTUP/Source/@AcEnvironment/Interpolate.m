function InterpEnv = Interpolate(Env1, Env2, R1, R2, RInterp);
%This function horizontally interpolates between environemts Env1 and Env2 at ranges R1 and R2 to produce a
%cell array of environments at ranges given in vector RInterp.
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

N1 = GetNumLayers(Env1);
N2 = GetNumLayers(Env2);

NLayer = min(N1, N2);

NRange = length(RInterp);
InterpEnv = [];
for ILayer = 1:NLayer
    Layer1 = GetLayer(Env1, ILayer);
    Layer2 = GetLayer(Env2, ILayer);
    
    %The Layer Interpolate method returns a cell array of layers - one for each range
    LayerArr{ILayer} = Interpolate(Layer1, Layer2, R1, R2, RInterp);
end

%Need to reorganise result so we can build the required environment objects
for IRange = 1:NRange
    P.LayerArr = [];
    for ILayer = 1:NLayer
        Temp = LayerArr{ILayer};
        P.LayerArr{ILayer} = Temp{IRange};
    end
    P.Name = [Env1.Name ' - Interp ' num2str(RInterp(IRange))];
    InterpEnv{IRange} = AcEnvironment(P, Env1.DirInfo);
end


    
