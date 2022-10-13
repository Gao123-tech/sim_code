function AcEnvCallback()
%Callback function for acoustic environment GUI interface
%Action is determined by tag.
%
%This really should go in the @AcEnvironment method directory but Matlab can't find it there.

[Cntrl, Fig] = gcbo;

MyData = get(Fig, 'UserData');
Obj = MyData.Obj;

ThisTag = get(Cntrl, 'Tag');

switch ThisTag
case 'AcEnvName',
   Obj = SetName(Obj, get(Cntrl, 'String'));
   
case 'MinBottomBounce',
   Obj = SetMinBottomBounce(Obj, str2num(get(Cntrl, 'String')));
      
case 'LayerList',
   switch get(Fig, 'SelectionType')
   case 'open',
	   List = findobj(Fig, 'Tag', 'LayerList');
   
   	Index = get(List, 'Value');
	   if Index >= 1
   	   Obj = EditLayer(Obj, Index);
      end
   end
   
      
   
case {'EditLayer', 'InsertBefore', 'InsertAfter', 'MoveUp', 'MoveDown'},
   List = findobj(Fig, 'Tag', 'LayerList');
   
   Index = get(List, 'Value');
   if Index >= 1
      switch ThisTag
      case 'EditLayer',
         Obj = EditLayer(Obj, Index);
      case {'InsertBefore','InsertAfter'},
         Obj = InsertLayer(Obj, Index, ThisTag);
      case {'MoveUp', 'MoveDown'},
         Obj = MoveLayer(Obj, Index, ThisTag);
      end
      
   end
   
   
case 'DeleteLayer',
   List = findobj(Fig, 'Tag', 'LayerList');
   
   Index = get(List, 'Value');
   if Index >= 1
      Obj = DeleteLayer(Obj, Index);
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
case {'EditLayer', 'DeleteLayer', 'LayerList', 'InsertBefore', 'InsertAfter', 'MoveUp', 'MoveDown'},
   if strcmpi(ThisTag, 'LayerList')
      if strcmpi(get(Fig, 'SelectionType'), 'open')
         Redraw = 1;
      else
         Redraw = 0;
      end
   else
      Redraw = 1;
   end
   
   if Redraw
	   %Re-draw the list of layers
   	List = findobj(Fig, 'Tag', 'LayerList');
      Str = GetLayerNames(Obj);
      set(List, 'Value', 1);
		set(List, 'String', Str);
   end
   
case {'OKButton', 'CancelButton'},
   uiresume(Fig);
end

