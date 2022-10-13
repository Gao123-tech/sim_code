function  Obj = EditLayer(Obj, Index);
Obj.RangeVec(Index) = SpecifyRange(Obj, Obj.RangeVec(Index));
Obj.EnvArr{Index} = EditGUI(Obj.EnvArr{Index});