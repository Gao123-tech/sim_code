function Obj = MoveLayer(Obj, Index, ThisTag)

DoSwap = 0;
NLayer = length(Obj.LayerArr);

switch ThisTag
case 'MoveUp',
   SwapInd = Index -1;
   if SwapInd >= 1
      DoSwap = 1;
   end
   
case 'MoveDown',
   SwapInd = Index + 1;
   if SwapInd <= NLayer
      DoSwap = 1;
   end
end

if DoSwap
   Temp = Obj.LayerArr{Index};
   Obj.LayerArr{Index} = Obj.LayerArr{SwapInd};
   Obj.LayerArr{SwapInd} = Temp;
end

   
   