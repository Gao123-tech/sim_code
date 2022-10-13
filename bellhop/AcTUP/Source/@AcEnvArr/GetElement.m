function [AcEnv, Range] = GetElement(Obj, Index)
if Index <= length(Obj.RangeVec)
    AcEnv = Obj.EnvArr{Index};
    Range = Obj.RangeVec(Index);
else
    AcEnv = [];
    Range = [];
end