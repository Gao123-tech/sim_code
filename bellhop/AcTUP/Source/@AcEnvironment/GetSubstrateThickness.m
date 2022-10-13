function [Zsub, HalfSpaceIsIt] = GetSubstrateThickness(EnvObj)
% Returns overal thickness of specified environment excluding the water
% column if HalfSpaceIsIt is false (0)
% If HalfSpaceIsIt is true (1) then it returns the thickness of the substrate
% layers, excluding the last (semi-infinite) layer
% Also Zsub  
%        = []           if EnvObj.LayerArray is empty
%        = 0            if the entire substrate is a half-space
%
% Revision 0     02 June 2003 - ALM
% Revision 0.01  03 July 2003 - ALM
%                - bug fix ... syntax error in GetLayerThickness call


% initialise 
true          = 1;
false         = 0;
HalfSpaceIsIt = false;

nlayers       = GetNumLayers(EnvObj);
if nlayers <= 0                               % if no layers
   Zsub = [];   
   bottom_idx  = 0;
elseif IsHalfSpace(GetLayer(EnvObj, 'last'))  % if semi-infinite
   HalfSpaceIsIt = true;
   bottom_idx  = nlayers - 1;
else
   bottom_idx = nlayers;
end

% loops from 1st substrate layer to last finite layer and accumulate layer thickness
Zsub = 0;
for layer_idx = 2:bottom_idx
   Zsub = Zsub + GetLayerThickness(GetLayer(EnvObj, layer_idx));
end

