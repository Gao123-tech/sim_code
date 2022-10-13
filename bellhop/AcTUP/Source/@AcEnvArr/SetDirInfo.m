function Obj = SetDirInfo(Obj, DirInfo)
for IEnv = 1:length(Obj.EnvArr)
    Obj.EnvArr{IEnv} = SetDirInfo(Obj.EnvArr{IEnv}, DirInfo);
end
