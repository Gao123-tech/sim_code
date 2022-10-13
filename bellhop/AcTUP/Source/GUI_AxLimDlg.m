function Out = GUI_AxLimDlg(message,OptionFlags, AxesHandles)
%AXLIMDLG Axes limits dialog box.
%   FIG = GUI_AxLimDlg(DlgName,OptionFlags, AxesHandles)
%   creates an axes limit dialog box with name DlgName and with prompts 
%   given by the rows of PromptString.
%   OptionFlags  - row vector of flags
%                  1st column: Auto limits checkbox (1=yes, 0=no)
%                  2nd column: Log scaling checkbox (1=yes, 0=no)
%                  Default is both off.
%   AxesHandles  - a vector of handles of the axes to which the selected limits are to be applied 
%
%Modified version of the Matlab axlimdlg function
%A J Duncan, CMST 15/12/00

ni = nargin;
if ni==0,
   message = 'Axes Limits';
end

% Check if figure is already on screen
if  figflag(message),
   % No need to create new dialog
   return

elseif strcmp(message,'EditCallback'),
   % Check to make sure limits are admissible
   fig = get(0,'CurrentFigure');
   Edit = get(fig,'CurrentObject');

   % Add brackets so that user does not have to.  Should work either way now.
   EditStr = ['[' get(Edit,'String') ']'];
   lim = eval(EditStr,'''foobar''');

   error_str = '';
   if isstr(lim),
      error_str = 'Cannot evaluate edit field input.';
   elseif any(size(lim)~=[1 2]),
      error_str = 'Axis limits must be a 1x2 vector.';
   elseif lim(2)<=lim(1),
      error_str = 'In the 1x2 input, [Lmin Lmax], Lmin must be less than Lmax.';
   end
   if isempty(error_str),
      set(Edit,'UserData',lim)
      % Also uncheck the associated Auto Checkbox (if it exists) 
      ud = get(fig,'UserData');
      ind = find(ud(:,2)==Edit);
      if ud(ind,3)~=0,
         set(ud(ind,3),'Value',0)
      end
   else
      errordlg(error_str);
      set(Edit,'String',mat2str(get(Edit,'UserData')))
   end
   return

elseif strcmp(message,'ApplyCallback'),
   % Loop and set axes limits
   fig = get(0,'CurrentFigure');
   ud = get(fig,'UserData');
   XYZstring = get(findobj(fig,'Tag','XYZstring'),'UserData');
   for i=1:size(ud,1),
      ax = get(ud(i,1),'UserData');
      BooVec = ishandle(ax);
      if isempty(ax) | ~all(BooVec),
         error_str = str2mat('GUI_AXLIMDLG: Empty or invalid axes handles.', ...
          'Try closing and then re-opening the axes limit dialog.');
         errordlg(error_str);
         return
      end
      if ud(i,3)~=0,
         IsAutoChecked = get(ud(i,3),'Value');
      else
         IsAutoChecked = 0;
      end
      if IsAutoChecked,
         % AutoCheckbox is checked, so set appropriate axes LimMode(s) to auto
         if any(XYZstring(i,:)=='x'),
            set(ax,'XLimMode','auto')
            lim = get(ax(1),'XLim');
         end
         if any(XYZstring(i,:)=='y'),
            set(ax,'YLimMode','auto')
            lim = get(ax(1),'YLim');
         end
         if any(XYZstring(i,:)=='z'),
            set(ax,'ZLimMode','auto')
            lim = get(ax(1),'ZLim');
         end
         % Also set edit field to current limits
         set(ud(i,2),'String',mat2str(lim))
      else
         lim = get(ud(i,2),'UserData');
         % Set appropriate limits
         if any(XYZstring(i,:)=='x'),
            set(ax,'XLim',lim)
         end
         if any(XYZstring(i,:)=='y'),
            set(ax,'YLim',lim)
         end
         if any(XYZstring(i,:)=='z'),
            set(ax,'ZLim',lim)
         end
      end
      if ud(i,4)~=0,
         % There is a Log checkbox
         if get(ud(i,4),'Value'),
            Scale = 'log';
         else
            Scale = 'linear';
         end
         if any(XYZstring(i,:)=='x'),
            set(ax,'XScale',Scale)
         end
         if any(XYZstring(i,:)=='y'),
            set(ax,'YScale',Scale)
         end
         if any(XYZstring(i,:)=='z'),
            set(ax,'ZScale',Scale)
         end
      end
   end
   return

elseif strcmp(message,'CancelCallback'),
   % Just delete the dialog
   delete(get(0,'CurrentFigure'))
   return
end

%%%%%%%%%%  Rest of code only executes if Axes Limits dialog is NOT on Screen  %%%%%%%%%%

DlgName = message;
% Error checking
% axlimdlg(message,OptionFlags,PromptString,AxesHandles,XYZstring,DefLim,ApplyFcn)
if ni<2,
   OptionFlags = [0 0];
elseif size(OptionFlags,2)~=2,
   error('OptionFlags must have 2 columns.')
end

if ni<3,
   AxesHandles = gca;
end

PromptString = str2mat('X-axis range:','Y-axis range:','Z-axis range:');
View = get(gca,'View');
if View(2)==90,
   PromptString(3,:) = [];
end

TextSize = size(PromptString);
rows = TextSize(1);
XYZstring = ['x'; 'y';'z'];
XYZstring = XYZstring(rem((1:rows)-1,3)+1,:);

DefLim = [];
for i=1:rows,
   DefLim = [DefLim; get(AxesHandles(1),[XYZstring(i,:) 'Lim'])];
end
ApplyFcn = '';

OptionFlagsRows = size(OptionFlags,1);
if OptionFlagsRows==1,
   OptionFlags = OptionFlags(ones(1,rows),:);
elseif OptionFlagsRows~=rows,
   error('OptionFlags must have either one row or as many rows as PromptString.')
end

% Get layout parameters
layout
mLineHeight = mLineHeight+5;
BWH = [mStdButtonWidth mStdButtonHeight];

% Define default position
ScreenUnits = get(0,'Units');
set(0,'Unit','pixels');
ScreenPos = get(0,'ScreenSize');
set(0,'Unit',ScreenUnits);
mCharacterWidth = 7;
Voff = 5;
FigWH = fliplr(TextSize).*[mCharacterWidth 2*(BWH(2)+Voff)] ...
        +[2*(mEdgeToFrame+mFrameToText)+BWH(1)+mFrameToText mLineHeight+BWH(2)+2*Voff];
MinFigW = 2*(BWH(1) +mFrameToText + mEdgeToFrame);
FigWH(1) = max([FigWH(1) MinFigW]);
FigWH = min(FigWH,ScreenPos(3:4)-50);
Position = [(ScreenPos(3:4)-FigWH)/2 FigWH];

% Make the figure
DefUIBgColor = get(0,'DefaultUIControlBackgroundColor');
fig = figure('NumberTitle','off','Name',DlgName,'Units','pixels', ...
 'Position',Position,'MenuBar','none', ...
 'Color',DefUIBgColor,'Visible','off','HandleVisibility','callback');

% Make the 2 frame uicontrols
UIPos = mEdgeToFrame*[1 1 -2 -2] + [0 0 FigWH(1) BWH(2)+mLineHeight];
uicontrol(fig,'Style','frame','Position',UIPos);
UIPos = [UIPos(1:3)+[0 UIPos(4)+mEdgeToFrame 0] FigWH(2)-UIPos(4)-2*mEdgeToFrame];
xyzctl=uicontrol(fig,'Style','frame','Position',UIPos,'Tag','XYZstring','HandleVisibility','callback');

% Make the text, edit, and check uicontrol(s)
UIPos = [mEdgeToFrame+mFrameToText FigWH(2)-mEdgeToFrame-Voff ...
      FigWH(1)-2*mEdgeToFrame-2*mFrameToText mLineHeight];

ud = zeros(rows,4);

for i=1:rows,
   AutoCheck = OptionFlags(i,1);
   LogCheck  = OptionFlags(i,2);
   UIPos = UIPos - [0 BWH(2) 0 0];
   ax = AxesHandles;
   ud(i,1) = uicontrol(fig,'Style','text','String',PromptString(i,:),'Position',UIPos, ...
    'HorizontalAlignment','left','UserData',ax,'HandleVisibility','callback');
   CheckX = FigWH(1)-BWH(1)-mEdgeToFrame-mFrameToText;
   if AutoCheck | LogCheck,
      if AutoCheck,
         String = 'Auto';
         ind = 3;
         Value = strcmp(get(ax(1),[XYZstring(i,1) 'LimMode']),'auto');
      else
         String = 'Log';
         ind = 4;
         Value = strcmp(get(ax(1),[XYZstring(i,1) 'Scale']),'log');
      end
      ud(i,ind) = uicontrol(fig,'Style','check','String',String, ...
       'Position',[CheckX UIPos(2) BWH],'HorizontalAlignment','left', ...
       'Value',Value,'HandleVisibility','callback');
   end

   UIPos = UIPos - [0 BWH(2)+Voff 0 0];
   EditStr = mat2str(DefLim(i,:));
   if LogCheck & AutoCheck,
      Value = strcmp(get(ax(1),[XYZstring(i,1) 'Scale']),'log');
      ud(i,4) = uicontrol(fig,'Style','check','String','Log', ...
       'Position',[CheckX UIPos(2) BWH],'HorizontalAlignment','left', ...
       'Value',Value,'HandleVisibility','callback');
      EditPos = UIPos -[0 0 BWH(1)+mFrameToText 0];
   else
      EditPos = UIPos;
   end
   ud(i,2) = uicontrol(fig,'Style','edit','String',EditStr,'BackgroundColor','white', ...
    'Position',EditPos,'HorizontalAlignment','left', ...
    'UserData',DefLim(i,:),'Callback','GUI_AxLimDlg(''EditCallback'')','HandleVisibility','callback');
   UIPos = UIPos -[0 Voff 0 0];
end
set(xyzctl,'UserData',XYZstring)

% Make the pushbuttons
Hspace = (FigWH(1)-2*BWH(1))/3;
ApplyFcn = [ApplyFcn 'GUI_AxLimDlg(''ApplyCallback'');'];
uicontrol(fig,'Style','push','String','Apply','Callback',ApplyFcn, ...
 'Position',[Hspace mLineHeight/2 BWH],'HandleVisibility','callback');
uicontrol(fig,'Style','push','String','Close','Callback','delete(gcf)', ...
 'Position',[2*Hspace+BWH(1) mLineHeight/2 BWH],'HandleVisibility','callback');

% Finally, make all the uicontrols normalized and the figure visible
set(get(fig,'Children'),'Unit','norm');
set(fig,'Visible','on','UserData',ud)

no = nargout;
if no,
   Out = fig;
end

% end GUI_axlimdlg
