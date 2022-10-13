function Obj = InsertLayer(Obj, Index, Dirn)
NLayer = length(Obj.LayerArr);
switch Dirn
case 'InsertBefore'
   NewIndex = Index;
   if NewIndex < 1
      NewIndex = 1;
   end
case 'InsertAfter',
   NewIndex = Index + 1;
end

for ThisInd = NLayer:-1:NewIndex
   Obj.LayerArr{ThisInd+1} = Obj.LayerArr{ThisInd};
end
Obj.LayerArr{NewIndex} = EditGUI(AcLayer);
