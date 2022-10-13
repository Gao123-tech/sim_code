function display(p)
%Display routine for AcEnvironment objects
disp(['Acoustic environment: ' p.Name]);
%disp(['MinBottomBounce: ' int2str(p.MinBottomBounce)]);
disp(['Gravity: ' int2str(p.Gravity)]);
disp(['PVapour: ' int2str(p.PVapour)]);
disp(['PAtm: ' int2str(p.PAtm)]);

NLayer = length(p.LayerArr)
for ILayer = 1:NLayer
   p.LayerArr{ILayer}
end

