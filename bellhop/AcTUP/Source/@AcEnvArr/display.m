function display(p)
%Display routine for AcEnvironment objects
disp(['Acoustic environment array: ' p.Name]);
%disp(['MinBottomBounce: ' int2str(p.MinBottomBounce)]);

NEnv = length(p.EnvArr);
for IEnv = 1:NEnv
   disp(' ');
    disp('-------------------------------------------------------------------------------------------------');
   disp(['Environment ' int2str(IEnv) ', Range = ' num2str(p.RangeVec(IEnv)) 'm']);
   disp(' ');
   p.EnvArr{IEnv}
end

