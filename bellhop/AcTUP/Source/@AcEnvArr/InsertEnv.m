function Obj = InsertEnv(Obj, Index, Dirn)
NEnv = length(Obj.EnvArr);
CurrentEnv = Obj.EnvArr{Index};
CurrentRange = Obj.RangeVec(Index);

switch Dirn
case 'InsertBefore'
   NewIndex = Index;
   if NewIndex < 1
      NewIndex = 1;
   end
case 'InsertAfter',
   NewIndex = Index + 1;
end

for ThisInd = NEnv:-1:NewIndex
   Obj.EnvArr{ThisInd+1} = Obj.EnvArr{ThisInd};
   Obj.RangeVec(ThisInd+1) = Obj.RangeVec(ThisInd);
end

Obj.RangeVec(NewIndex) = SpecifyRange(Obj, CurrentRange);

Obj.EnvArr{NewIndex} = EditGUI(CurrentEnv);
