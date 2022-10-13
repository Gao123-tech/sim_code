function fig = DrawEditGUI(Obj)

%load EditGUI
	%'Colormap',mat0, ...
	%'PointerShapeCData',mat1, ...

%Figure window
h0 = figure('Color',[0.8 0.8 0.8], ...
	'Units','points', ...
	'MenuBar','none', ...
	'Name','Edit acoustic environment', ...
	'Position',[200 100 330 300], ...
	'Tag','Fig1');

MyData.IsDone = 0;
MyData.IsOK = 0;
MyData.Obj = Obj;
set(h0, 'UserData', MyData);

%Static text - environment name
h1 = uicontrol('Parent',h0, ...
	'Units','points', ...
	'BackgroundColor',[0.752941176470588 0.752941176470588 0.752941176470588], ...
	'HorizontalAlignment','left', ...
	'ListboxTop',0, ...
	'Position',[33 245 171.75 13.5], ...
	'String','Acoustic environment name:', ...
	'Style','text', ...
   'Tag','StaticText1');

%Name edit control
h1 = uicontrol('Parent',h0, ...
	'Units','points', ...
	'BackgroundColor',[1 1 1], ...
	'Callback','AcEnvCallback', ...
	'ListboxTop',0, ...
	'Position',[33 230 172.5 15], ...
   'Style','edit', ...
   'String', Obj.Name, ...
   'HorizontalAlignment', 'left', ...
   'Tag','AcEnvName');
   


%Static text - layer list title
h1 = uicontrol('Parent',h0, ...
	'Units','points', ...
	'BackgroundColor',[0.752941176470588 0.752941176470588 0.752941176470588], ...
	'HorizontalAlignment','left', ...
	'ListboxTop',0, ...
	'Position',[33 200 57 15.75], ...
	'String','Layers:', ...
	'Style','text', ...
   'Tag','StaticText2');

%List of current layers
Str = GetLayerNames(Obj);

h1 = uicontrol('Parent',h0, ...
	'Units','points', ...
	'BackgroundColor',[1 1 1], ...
	'Callback','AcEnvCallback', ...
	'Position',[30 60 175 140], ...
	'String', Str, ...
	'Style','listbox', ...
	'Tag','LayerList', ...
   'Value',1);


%Edit button
h1 = uicontrol('Parent',h0, ...
	'Units','points', ...
	'Callback','AcEnvCallback', ...
	'ListboxTop',0, ...
	'Position',[220 185 80 15], ...
	'String','Edit layer', ...
   'Tag','EditLayer');

%Delete layer button
h1 = uicontrol('Parent',h0, ...
	'Units','points', ...
	'Callback','AcEnvCallback', ...
	'ListboxTop',0, ...
	'Position',[220 165 80 15], ...
	'String','Delete layer', ...
   'Tag','DeleteLayer');

%Insert before button
h1 = uicontrol('Parent',h0, ...
	'Units','points', ...
	'Callback','AcEnvCallback', ...
	'ListboxTop',0, ...
	'Position',[220 130 80 15], ...
	'String','Insert layer before', ...
   'Tag','InsertBefore');

%Insert after button
h1 = uicontrol('Parent',h0, ...
	'Units','points', ...
	'Callback','AcEnvCallback', ...
	'ListboxTop',0, ...
	'Position',[220 110 80 15], ...
	'String','Insert layer after', ...
   'Tag','InsertAfter');

%Move layer up button
h1 = uicontrol('Parent',h0, ...
	'Units','points', ...
	'Callback','AcEnvCallback', ...
	'ListboxTop',0, ...
	'Position',[220 75 80 15], ...
	'String','Move layer up', ...
   'Tag','MoveUp');

%Move layer down button
h1 = uicontrol('Parent',h0, ...
	'Units','points', ...
	'Callback','AcEnvCallback', ...
	'ListboxTop',0, ...
	'Position',[220 55 80 15], ...
	'String','Move layer down', ...
   'Tag','MoveDown');

%OK button
h1 = uicontrol('Parent',h0, ...
	'Units','points', ...
	'Callback','AcEnvCallback', ...
	'ListboxTop',0, ...
	'Position',[162 16.5 45 15], ...
	'String','OK', ...
   'Tag','OKButton');

%Cancel button
h1 = uicontrol('Parent',h0, ...
	'Units','points', ...
	'Callback','AcEnvCallback', ...
	'ListboxTop',0, ...
	'Position',[34.5 15.75 45 15], ...
	'String','Cancel', ...
	'Tag','CancelButton');
if nargout > 0, fig = h0; end
