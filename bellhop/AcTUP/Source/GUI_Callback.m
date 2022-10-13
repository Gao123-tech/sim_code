function GUI_Callback()

Action = get(gcbo, 'Tag');


switch(Action)
    case 'RescaleCurrent',
        %Test to see whether this set of axes is a colorbar
        ImgHndl = findobj(gca, 'type', 'image', 'tag', 'TMW_COLORBAR');
        if ~isempty(ImgHndl)
            ud = get(gca,'userdata');
            axes(ud.PlotHandle);
        end
        AxDlg = GUI_AxLimDlg('Limits for current axes', [1 1]);
        uiwait(AxDlg);

    case {'SaveTIFPortrait', 'SaveTIFLandscape', 'SaveTIFLineArt'},
        %Set up the paper position properties appropriate for the specified orientation
        ThisFig = gcbf;
        switch Action
            case 'SaveTIFPortrait',
                set(ThisFig, ...
                    'PaperType', 'A4', ...
                    'PaperUnits', 'centimeters', ...
                    'PaperOrientation', 'portrait', ...
                    'PaperPosition', [3.17, 16 ,13.5, 10.125] ...
                    );
                PrtOpt = '-dtiff';
                ResOpt = '-r0';

            case 'SaveTIFLandscape',
                set(ThisFig, ...
                    'PaperType', 'A4', ...
                    'PaperUnits', 'centimeters', ...
                    'PaperOrientation', 'landscape', ...
                    'PaperPosition', [2.5, 3.17, 24, 13.625] ...
                    );
                PrtOpt = '-dtiff';
                ResOpt = '-r0';
                %'PaperPosition', [5.072, 3.17, 18.166667, 13.625] ...

            case 'SaveTIFLineArt',
                %One column width at 600 dpi
                set(ThisFig, ...
                    'PaperType', 'A4', ...
                    'PaperUnits', 'centimeters', ...
                    'PaperOrientation', 'portrait', ...
                    'PaperPosition', [3.17, 16 ,13.5, 10.125] ...
                    );
                PrtOpt = '-dtiffnocompression';
                ResOpt = '-r390';

        end
        if exist('GUI_Defaults.m', 'file')
            DfltInfo = GUI_Defaults;
            Home = pwd;
            cd(DfltInfo.ImageDir);
            SetDflt = 1;
        else
            SetDflt = 0;
        end

        [FName, Path] = uiputfile('*.tif', 'Save figure to:');
        if SetDflt
            cd(Home);
        end

        if FName ~= 0
            HFig = gcbf;
            %print('-dtiff', '-fHFig', [Path FName]);
            print(PrtOpt, ResOpt, [Path FName]);

        end

    case 'RescaleAll',
        AxesHandles = findobj(gcbf, 'Type', 'axes');
        NAx = length(AxesHandles);
        GotNewVals = 0;

        AxArr = [];
        for IAx = 1:NAx
            %Test to see whether this set of axes is a colorbar
            ImgHndl = findobj(AxesHandles(IAx), 'type', 'image', 'tag', 'TMW_COLORBAR');

            %don't rescale the colorbars
            if isempty(ImgHndl)
                AxArr = [AxArr AxesHandles(IAx)];
            end
        end

        if ~isempty(AxArr)
            AxDlg = GUI_AxLimDlg('Limits for all axes', [1 1], AxArr);
            uiwait(AxDlg);
        end


    case 'Zoom',
        CtrlType = get(gcbo, 'Type');
        switch CtrlType
            case 'uimenu',
                if strcmp(get(gcbo, 'Checked'), 'on')
                    ZoomState = 0;
                    set(gcbo, 'Checked', 'off');
                else
                    ZoomState = 1;
                    set(gcbo, 'Checked', 'on');
                end

            case 'uicontrol',
                ZoomState = get(gcbo, 'Value');
        end

        if ZoomState;
            zoom on;
        else
            zoom off;
        end

    case 'Rotate3D',
        CtrlType = get(gcbo, 'Type');
        switch CtrlType
            case 'uimenu',
                if strcmp(get(gcbo, 'Checked'), 'on')
                    RotState = 0;
                    set(gcbo, 'Checked', 'off');
                else
                    RotState = 1;
                    set(gcbo, 'Checked', 'on');
                end

            case 'uicontrol',
                RotState = get(gcbo, 'Value');
        end
        if RotState;
            rotate3d on;
        else
            rotate3d off;
        end

    case {'ColourRescaleAll', 'ColourRescaleCurrent'},
        Current = caxis;
        prompt={'Enter new minimum value:','Enter new maximum value:'};
        def={num2str(Current(1)), num2str(Current(2))};
        ThisTitle='Colour axis scaling';
        lineNo=1;
        OK = 0;
        Cancel = 0;
        while ~OK & ~Cancel
            Answer=inputdlg(prompt,ThisTitle,lineNo,def);
            if isempty(Answer)
                Cancel = 1;
            else

                Min = str2num(Answer{1});
                Max = str2num(Answer{2});
                if ~isempty(Min) & ~isempty(Max)
                    OK = 1;
                else
                    if isempty(Min)
                        Warn = warndlg('Invalid minimum value', '');
                        uiwait(Warn);
                    end

                    if isempty(Max)
                        Warn = warndlg('Invalid maximum value', '');
                        uiwait(Warn);
                    end
                end
            end

        end
        if OK

            switch Action
                case 'ColourRescaleAll',
                    AxesHandles = findobj(gcbf, 'Type', 'axes');

                    for IAx = 1:length(AxesHandles)
                        if ~lIsColorbar(AxesHandles(IAx))
                            lUpdateColors(AxesHandles(IAx), Min, Max);
                        end
                    end


                case 'ColourRescaleCurrent',
                    %Test to see whether this set of axes is a colorbar
                    if lIsColorbar(gca)
                        DefAx = lFindDefiningAxes(gca);
                    else
                        DefAx = gca;
                    end

                    if ~isempty(DefAx)
                        lUpdateColors(DefAx, Min, Max);
                    end

            end
        end



    case 'GInput',
        Done = 0;
        while ~Done
            figure(gcbf);
            [X, Y, Button] = GUI_ginput(1);
            Status = GUI_GInputQuery({['X = ', num2str(X, 9)], ['Y = ', num2str(Y, 9)]});
            Done = ~Status;
        end
        if strcmp(get(gcbo, 'type'), 'uicontrol')
            set(gcbo, 'value', 0);
        end

    case 'DeltaValues',
        Done = 0;
        while ~Done
            figure(gcbf);
            [X, Y, Button] = GUI_ginput(2);
            Status = GUI_GInputQuery({['Delta X = ', num2str(diff(X), 9)], ...
                ['Delta Y = ', num2str(diff(Y), 9)], ...
                ['Distance = ', num2str(sqrt(diff(X)^2 + diff(Y)^2))]});
            Done = ~Status;
        end
        if strcmp(get(gcbo, 'type'), 'uicontrol')
            set(gcbo, 'value', 0);
        end


    case 'FontEverythingAll',
        AxesHandles = findobj(gcbf, 'Type', 'axes');
        for IAx = 1:length(AxesHandles);
            if IAx == 1
                NewFont = uisetfont(AxesHandles(IAx));
            else
                set(AxesHandles(IAx), NewFont);
            end
            HArr = get(AxesHandles(IAx), {'xlabel', 'ylabel', 'zlabel', 'title'});
            for ILab = 1:length(HArr)
                set(HArr{ILab}, NewFont);
            end
        end

    case 'FontAxesAll',
        AxesHandles = findobj(gcbf, 'Type', 'axes');
        for IAx = 1:length(AxesHandles);
            if IAx == 1
                NewFont = uisetfont(AxesHandles(IAx));
            else
                set(AxesHandles(IAx), NewFont);
            end
        end

    case 'FontLabelsAll',
        AxesHandles = findobj(gcbf, 'Type', 'axes');
        for IAx = 1:length(AxesHandles);
            HArr = get(AxesHandles(IAx), {'xlabel', 'ylabel', 'zlabel'});
            for ILab = 1:length(HArr)
                if (IAx == 1) & (ILab == 1)
                    NewFont = uisetfont(HArr{ILab});
                else
                    set(HArr{ILab}, NewFont);
                end
            end
        end

    case 'FontTitleAll',
        AxesHandles = findobj(gcbf, 'Type', 'axes');
        for IAx = 1:length(AxesHandles);
            h = get(AxesHandles(IAx), 'title');
            if (IAx == 1)
                NewFont = uisetfont(h);
            else
                set(h, NewFont);
            end
        end

    case 'FontDefaultAll'
        DfltInfo = GUI_Defaults;
        AxesHandles = findobj(gcbf, 'Type', 'axes');
        for IAx = 1:length(AxesHandles);
            set(AxesHandles(IAx), DfltInfo.AxisFont);
            HArr = get(AxesHandles(IAx), {'xlabel', 'ylabel', 'zlabel', 'title'});
            for ILab = 1:3
                set(HArr{ILab}, DfltInfo.LabelFont);
            end
            set(HArr{4}, DfltInfo.TitleFont);
        end

    case 'FontEverythingCurrent',
        Ax = get(gcbf, 'CurrentAxes');
        NewFont = uisetfont(Ax);
        HArr = get(Ax, {'xlabel', 'ylabel', 'zlabel', 'title'});
        for ILab = 1:length(HArr)
            set(HArr{ILab}, NewFont);
        end

    case 'FontAxesCurrent',
        Ax = get(gcbf, 'CurrentAxes');
        NewFont = uisetfont(Ax);

    case 'FontLabelsCurrent',
        Ax = get(gcbf, 'CurrentAxes');
        HArr = get(Ax, {'xlabel', 'ylabel', 'zlabel'});
        for ILab = 1:length(HArr)
            if (ILab == 1)
                NewFont = uisetfont(HArr{ILab});
            else
                set(HArr{ILab}, NewFont);
            end
        end

    case 'FontTitleCurrent',
        Ax = get(gcbf, 'CurrentAxes');
        h = get(Ax, 'title');
        NewFont = uisetfont(h);

    case 'NewTitleCurrent',
        Ax = get(gcbf, 'CurrentAxes');
        h = get(Ax, 'title');
        CurrentTitle = get(h, 'String');
        Ans = inputdlg({'New title:'}, '', 1, {CurrentTitle});
        if ~isempty(Ans)
            set(h, 'String', Ans{1});
        end

    case 'FontDefaultCurrent'
        DfltInfo = GUI_Defaults;
        Ax = get(gcbf, 'CurrentAxes');
        set(Ax, DfltInfo.AxisFont);
        HArr = get(Ax, {'xlabel', 'ylabel', 'zlabel', 'title'});
        for ILab = 1:3
            set(HArr{ILab}, DfltInfo.LabelFont);
        end
        set(HArr{4}, DfltInfo.TitleFont);

    case 'GridOnOffCurrent',
        Ax = get(gcbf, 'CurrentAxes');
        axes(Ax);
        grid;
        GridState = strcmp(get(Ax, 'XGrid'), 'on');
        if GridState;
            set(gcbo, 'Checked', 'on');
        else
            set(gcbo, 'Checked', 'off');
        end

    case 'GridOnOffAll',
        AxesHandles = findobj(gcbf, 'Type', 'axes');
        for IAx = 1:length(AxesHandles)
            Ax = AxesHandles(IAx);
            axes(Ax);
            if IAx == 1
                grid;
                GridState = strcmp(get(Ax, 'XGrid'), 'on');
                if GridState;
                    set(gcbo, 'Checked', 'on');
                else
                    set(gcbo, 'Checked', 'off');
                end
            else
                if GridState
                    grid on;
                else
                    grid off;
                end
            end
        end

    case 'BoxOnOffCurrent',
        Ax = get(gcbf, 'CurrentAxes');
        axes(Ax);
        box;
        BoxState = strcmp(get(Ax, 'Box'), 'on');
        if BoxState;
            set(gcbo, 'Checked', 'on');
        else
            set(gcbo, 'Checked', 'off');
        end

    case 'BoxOnOffAll',
        AxesHandles = findobj(gcbf, 'Type', 'axes');
        for IAx = 1:length(AxesHandles)
            Ax = AxesHandles(IAx);
            axes(Ax);
            if IAx == 1
                box;
                BoxState = strcmp(get(Ax, 'Box'), 'on');
                if BoxState;
                    set(gcbo, 'Checked', 'on');
                else
                    set(gcbo, 'Checked', 'off');
                end
            else
                if BoxState
                    box on;
                else
                    box off;
                end
            end
        end
    case 'NewTitleAll',
        StartChar = 'a';
        AxesHandles = findobj(gcbf, 'Type', 'axes');
        NAx = length(AxesHandles);
        Position = zeros(NAx, 4);
        Index = 1:NAx;

        for IAx = 1:NAx
            Ax = AxesHandles(IAx);
            axes(Ax);
            Position(IAx, :) = get(Ax, 'Position');
        end

        Done = 0;
        RefPt = [0.0 1.0];
        AxNum = 1;
        while ~isempty(Position)
            %Find the first plot in the row
            TopLeft = [Position(:, 1), Position(:,2)+Position(:,4)];
            DistFromRef = sqrt((TopLeft(:, 1)-RefPt(1)).^2 + (TopLeft(:,2)-RefPt(2)).^2);
            [MinDist, IRef] = min(DistFromRef);  %Find the top-left plot
            RefPosn = Position(IRef, :);
            RefHandle = AxesHandles(IRef);

            Sz = size(Position);
            %Look for something to the right of the current plot

            ThisPosn = [];
            ThisHandle = [];
            ThisIndex = [];

            for ISub = 1:Sz(1)
                if lIsYOverlapped(Position(ISub, :), RefPosn)
                    ThisPosn = [ThisPosn; Position(ISub, :)];
                    ThisHandle = [ThisHandle; AxesHandles(ISub)];
                    ThisIndex = [ThisIndex; Index(ISub)];
                end
            end

            HDist = ThisPosn(:, 1) - RefPosn(1);
            [Dist, ColInd] = sort(HDist);

            for ICol = 1:length(ColInd)
                Ax = ThisHandle(ColInd(ICol));
                axes(Ax);
                h = get(Ax, 'title');
                ThisChar = char(double(StartChar)+AxNum-1);
                set(h, 'String', ['(' ThisChar ')']);
                AxNum = AxNum + 1;
            end

            if ~isempty(ThisIndex)
                Position(ThisIndex, :) = [];
                AxesHandles(ThisIndex) = [];
                Index(ThisIndex) = [];
                RefPt = [RefPosn(1) RefPosn(2)];
            end

        end


    case 'LineWidthAll',
        prompt={'Enter new line width:'};
        title='';
        lineNo=1;
        AxesHandles = findobj(gcbf, 'Type', 'axes');
        for IAx = 1:length(AxesHandles);
            HArr = get(AxesHandles(IAx), 'Children');
            for ICh = 1:length(HArr)
                if (IAx == 1) & (ICh == 1)
                    def = {num2str(get(HArr(ICh), 'linewidth'))};
                    Answer=inputdlg(prompt,title,lineNo,def);
                    if ~isempty(Answer)
                        NewWidth = str2num(Answer{1});
                    else
                        NewWidth = [];
                    end
                end
                if ~isempty(NewWidth)
                    set(HArr(ICh), 'linewidth', NewWidth);
                end

            end
        end

    case 'LineWidthCurrent',
        prompt={'Enter new line width:'};
        title='';
        lineNo=1;
        Ax = get(gcbf, 'CurrentAxes');
        HArr = get(Ax, 'Children');
        for ICh = 1:length(HArr)
            if  (ICh == 1)
                def = {num2str(get(HArr(ICh), 'linewidth'))};
                Answer=inputdlg(prompt,title,lineNo,def);
                if ~isempty(Answer)
                    NewWidth = str2num(Answer{1});
                else
                    NewWidth = [];
                end
            end
            if ~isempty(NewWidth)
                set(HArr(ICh), 'linewidth', NewWidth);
            end
        end

end


function Properties = SaveAxisProperties(Ax)

%Define properties to save
AxisProperties = {'fontname', 'fontsize', 'fontunits', 'fontweight', 'fontangle'};
LabelProperties = {'string', 'fontname', 'fontsize', 'fontunits', 'fontweight', 'fontangle'};

%save specified axis properties
AxProp = get(Ax, AxisProperties);

%save label properties
hx = get(Ax, 'xlabel');
XStrProp = get(hx, LabelProperties);

hy = get(Ax, 'ylabel');
YStrProp = get(hy, LabelProperties);

hz = get(Ax, 'zlabel');
ZStrProp = get(hz, LabelProperties);

Properties.AxPropDef = AxisProperties;
Properties.LabelPropDef = LabelProperties;
Properties.Axis = AxProp;
Properties.XStr = XStrProp;
Properties.YStr = YStrProp;
Properties.ZStr = ZStrProp;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function RestoreAxisProperties(Ax, P)

set(Ax, P.AxPropDef, P.Axis);

hx = get(Ax, 'xlabel');
set(hx, P.LabelPropDef, P.XStr);

hy = get(Ax, 'ylabel');
set(hy, P.LabelPropDef, P.YStr);

hz = get(Ax, 'zlabel');
set(hz, P.LabelPropDef, P.ZStr);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function Flag = lIsXOverlapped(Posn1, Posn2)
Flag = ((Posn2(1) <= Posn1(1)) & (Posn2(1)+Posn2(3) >= Posn1(1))) | ...
    ((Posn2(1) <= Posn1(1)+Posn1(3)) & (Posn2(1)+Posn2(3) >= Posn1(1)+Posn1(3)));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function Flag = lIsYOverlapped(Posn1, Posn2)
Flag = ((Posn2(2) <= Posn1(2)) & (Posn2(2)+Posn2(4) >= Posn1(2))) | ...
    ((Posn2(2) <= Posn1(2)+Posn1(4)) & (Posn2(2)+Posn2(4) >= Posn1(2)+Posn1(4)));

%
%----------------------------------------------------%
function hc = lFindColorbar(peeraxes)

fig = get(peeraxes,'parent');
if ReleaseGT13
    ax = findobj(fig,'type','axes');
    hc=[];
    k=1;
    % vectorize
    while k<=length(ax) && isempty(hc)
        if lIsColorbar(ax(k))
            hax = handle(ax(k));
            if isequal(double(hax.axes),peeraxes)
                hc=ax(k);
            end
        end
        k=k+1;
    end
else
    AllCBarImg = findobj(fig, 'type', 'image', 'tag', 'TMW_COLORBAR');

    for IImg = 1:length(AllCBarImg)
        CB = get(AllCBarImg(IImg), 'Parent');  %The colorbar is the parent of the image
        UData = get(CB, 'userdata');
        if UData.PlotHandle == peeraxes
            hc = CB;
        end
    end

end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function DefAx = lFindDefiningAxes(CBHndl)
%Finds the axes that defined the colorbar axes with the specified handle
if ReleaseGT13
    DefAx = double(CBHndl.axes);
else
    UData = get(CBHndl, 'userdata');
    DefAx = UData.PlotHandle;
end


%----------------------------------------------------%
function tf=lIsColorbar(ax)

if ReleaseGT13
    if length(ax) ~= 1 || ~ishandle(ax)
        tf=false;
    else
        tf=isa(handle(ax),'scribe.colorbar');
    end
else
    ImgHndl = findobj(ax, 'type', 'image', 'tag', 'TMW_COLORBAR');
    if isempty(ImgHndl)
        tf = 0;
    else
        tf = 1;
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function lUpdateColors(DefAx, Min, Max);
%This function updates the plot colours and also the colours of any associated colorbar
axes(DefAx);
caxis([Min Max]);
ThisColorBar = lFindColorbar(DefAx);

if ~isempty(ThisColorBar)
    Prop = SaveAxisProperties(ThisColorBar);
    caxis([Min Max]);
    h = colorbar;  %re-draw colorbar

    %restore the axis and label properties
    RestoreAxisProperties(h, Prop);
end

function Val = ReleaseGT13()
%Handling of colorbars changed with release 14
Val = (str2num(version('-release')) > 13);
