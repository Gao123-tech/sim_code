function Layer = GetLayer(Obj, Index)
% - returns layer specified its index ... 1 corresponds to the 
%   uppermost layer
% - number of layers found by GetNumLayers
% - if number outside range or object has zero layers, returns []
% - for last layer put Index = 'last'

% 
Layer = [];
% do we have a valid object ?
if isempty(Obj.LayerArr)
    return;
else
    % does the object contain 1 or more layers ?
    IndexMax = GetNumLayers(Obj);
    if IndexMax < 1, 
        return; 
    end
end

% has a string been passed
if isstr(Index)
    % is it a legal string
    if strcmpi(Index, 'last')
        Index = IndexMax;
    else
        return;
    end
end
% otherwise its a number - test it
if Index < 1 | Index > IndexMax
    return;
end

%get layer
Layer = Obj.LayerArr{Index};
