function Str = GetEnvironmentNames(Obj)
if isempty(Obj.EnvArr)
  	Str = '';
else
  	for IEnv = 1:length(Obj.EnvArr)
     	Str{IEnv} = GetName(Obj.EnvArr{IEnv});
  	end
end
