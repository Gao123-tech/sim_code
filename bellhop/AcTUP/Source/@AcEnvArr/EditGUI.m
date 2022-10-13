function [Obj, Modified] = EditGUI(Obj)
%Allows interactive editing of an acoustic environment object

Done = 0;
Modified = 0;
while ~Done
	Fig = DrawEditGUI(Obj);

	uiwait(Fig);

	MyData = get(Fig, 'UserData');

	if MyData.IsOK
      Obj = MyData.Obj;
      Modified = 1;
   end

   IsGood = lCheckData(MyData);

   if IsGood
       Done = 1;
   end
   
   close(Fig);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function IsGood  = lCheckData(MyData)
IsGood = 1;
Obj = MyData.Obj;
NEl = GetNumberOfElements(Obj);
if NEl > 1
    RStep = diff(Obj.RangeVec);
    if ~isempty(find(RStep <= 0))
        uiwait(warndlg('Invalid input data - ranges must increase monotonically'));
        IsGood = 0;
    end
end
