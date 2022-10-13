function NewDir = PlotGreensFn(DefaultDir)
% NewDir = PlotGreensFn(DefaultDir)
% plots a Greens function file as output by SCOOTER
% Alec Duncan, May 2002
%
% Revision 0.1    23 March 2005 ... ALM
%                 - change figure number initiasation to next free figure number                 
%                 - add subplot selection 
%                 - without spending more than 30s thinking can't implement subplot and AJD's GraphMenu so this is now optional

MaxLegend = 10;  %Maximum number of legend rows

if nargin < 1
   ChangeDir = 0;
else
   StartDir = pwd;
   DirStat = 1;
   if ~exist(DefaultDir, 'dir')
       DirStat = LibMakeDirectory(DefaultDir);
   end
   if DirStat
       cd(DefaultDir);
       ChangeDir = 1;
   else
       ChangeDir = 0;
   end
end

[FName, Path] = uigetfile('*.grn', 'Select file to plot');

if ChangeDir
   cd(StartDir);
end


if FName == 0
    if nargin < 1
        NewDir = pwd;
    else
        NewDir = DefaultDir;
    end
else
    NewDir = Path;
        
    [Zs, Zr, Freq, Kr, GKr, Title] = ReadGreen([Path FName]);
    
    % Revision 0.1 - see notes in header
    [fhdls, current, next] = FindFigures();    
    %Ans = inputdlg({'Figure number'}, '', 1, {int2str(gcf)});
    Ans = inputdlg({'Figure number', 'Subplot vector', 'Add AJD''s GraphMenu 0/1 (forces subplot = [1 1 1])'}, ...
                    'Plot Green''s Function', 1, {int2str(next), '[1 1 1]', '0'});
    if isempty(Ans)
       return;
    end   
    FigNum = str2num(Ans{1});
    subvec = str2num(Ans{2});
    GMenu  = str2num(Ans{3});

    ZsTxt = ['Zs = ' num2str(Zs) 'm'];
    if length(Zr) == 1
        TitleTxt = [Title ', Zs = ' num2str(Zs) 'm, Zr = ' num2str(Zr) 'm, F = ' num2str(Freq), 'Hz'];
        DoLegend = 0;
    else
        TitleTxt = [Title ', Zs = ' num2str(Zs) 'm, F = ' num2str(Freq), 'Hz'];
        
        if length(Zr) <= MaxLegend
            LegendText = [];
            for IR = 1:length(Zr)
                LegendText{IR} = ['Zr = ' num2str(Zr(IR)) 'm'];
            end
            DoLegend = 1;
        else
            DoLegend = 0;
        end
    end

    figure(FigNum);
    subplot(subvec(1), subvec(2), subvec(3));
    if exist('GUI_SetupGraphMenu.m', 'file') & GMenu
        GUI_SetupGraphMenu('plot');
    end
    plot(Kr, abs(GKr(:, :, 1)));
    title(TitleTxt);
    xlabel('Horizontal wavenumber (m^-^1)');
    ylabel('Magnitude of Greens function');
    if DoLegend
        legend(LegendText);
    end
end