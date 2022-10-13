function AcEnvArrCallback()
%Callback function for acoustic environment array GUI interface
%Action is determined by tag.
%
%This really should go in the @AcEnvArr method directory but Matlab can't find it there.

[Cntrl, Fig] = gcbo;

MyData = get(Fig, 'UserData');
Obj = MyData.Obj;

ThisTag = get(Cntrl, 'Tag');
 
switch ThisTag
case 'AcEnvArrName',
   Obj = SetName(Obj, get(Cntrl, 'String'));
   
case 'RInterp'
    Obj = SetdRInterp(Obj, str2num(get(Cntrl, 'String')));
   
case 'EnvList',
   switch get(Fig, 'SelectionType')
   case 'open',
	   List = findobj(Fig, 'Tag', 'EnvList');
   
   	Index = get(List, 'Value');
	   if Index >= 1
   	   Obj = EditEnv(Obj, Index);
      end
   end
   
      
   
case {'EditEnv', 'InsertBefore', 'InsertAfter', 'MoveUp', 'MoveDown'},
   List = findobj(Fig, 'Tag', 'EnvList');
   
   Index = get(List, 'Value');
   if Index >= 1
      switch ThisTag
      case 'EditEnv',
         Obj = EditEnv(Obj, Index);
      case {'InsertBefore','InsertAfter'},
         Obj = InsertEnv(Obj, Index, ThisTag);
      case {'MoveUp', 'MoveDown'},
         Obj = MoveEnv(Obj, Index, ThisTag);
      end
      
   end
   
   
case 'DeleteEnv',
   List = findobj(Fig, 'Tag', 'EnvList');
   
   Index = get(List, 'Value');
   if Index >= 1
      Obj = DeleteEnv(Obj, Index);
   end
   
   
case 'OKButton',
   MyData.IsOK = 1;
   MyData.IsDone = 1;
   
case 'CancelButton',
   MyData.IsOK = 0;
   MyData.IsDone = 1;
end

MyData.Obj = Obj;
set(Fig, 'UserData', MyData);

switch ThisTag
case {'EditEnv', 'DeleteEnv', 'EnvList', 'InsertBefore', 'InsertAfter', 'MoveUp', 'MoveDown'},
   if strcmpi(ThisTag, 'EnvList')
      if strcmpi(get(Fig, 'SelectionType'), 'open')
         Redraw = 1;
      else
         Redraw = 0;
      end
   else
      Redraw = 1;
   end
   
   if Redraw
       %Re-draw the list of Environments
       List = findobj(Fig, 'Tag', 'EnvList');
       Str = GetEnvironmentNames(Obj);
       RangeVec = GetRangeVec(Obj);
       NEnv = length(RangeVec);
       for IEnv = 1:NEnv
           Str{IEnv} = [num2str(RangeVec(IEnv)), 'm,  ', Str{IEnv}];
       end
       
       set(List, 'Value', 1);
       set(List, 'String', Str);
   end
   
case {'OKButton', 'CancelButton'},
   uiresume(Fig);
end

