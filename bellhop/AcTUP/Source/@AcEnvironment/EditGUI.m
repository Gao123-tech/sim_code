function [Obj, Modified] = EditGUI(Obj, ForcePekeris)
%Allows interactive editing of an acoustic environment object
if nargin < 2
   ForcePekeris = 0;
end

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
   
   if ForcePekeris
      if IsPekeris(Obj)
         Done = 1;
      else
         h = warndlg('Environment is not a valid Pekeris environment');
         uiwait(h);
      end
   else
      Done = 1;
   end
   close(Fig);
end
