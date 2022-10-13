function Obj = MoveLayer(Obj, Index, ThisTag)

DoSwap = 0;
NEnv = length(Obj.EnvArr);

switch ThisTag
case 'MoveUp',
   SwapInd = Index -1;
   if SwapInd >= 1
      DoSwap = 1;
   end
   
case 'MoveDown',
   SwapInd = Index + 1;
   if SwapInd <= NEnv
      DoSwap = 1;
   end
end

if DoSwap
   Temp = Obj.EnvArr{Index};
   TempR = Obj.RangeVec(Index);
   Obj.EnvArr{Index} = Obj.EnvArr{SwapInd};
   Obj.RangeVec(Index) = Obj.RangeVec(SwapInd);
   Obj.EnvArr{SwapInd} = Temp;
   Obj.RangeVec(SwapInd) = TempR;
end

   
   