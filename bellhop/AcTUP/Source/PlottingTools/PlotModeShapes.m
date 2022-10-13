function NewDir = PlotModeShapes(DefaultDir)
% NewDir = PlotModeShapes(DefaultDir)
% plots the mode shapes produced by Kraken or Krakenc
% Alec Duncan, June 2002

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

[FName, Path] = uigetfile('*.mod', 'Select file to plot');

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
        
    [ Title, Freq, Kr, Z, Phi, cpt, cst, rhot, deptht, cpb, csb, rhob, depthb, Nmedia, depth, rho ] = ReadModes([Path FName]);
    
    if isempty(Kr)
        uiwait(warndlg('No modes to plot'));
        return;
    end
    
    Ans = inputdlg({['Mode number(s) to plot (1 to ' int2str(length(Kr)) ')'], 'Figure number', 'Include legend (y/n)'}, ...
        '', 1, {'1', int2str(gcf), 'y'});
    if isempty(Ans)
       return;
    end
   
    PltModes = str2num(Ans{1});
    FigNum = str2num(Ans{2});
    DoLegend = strcmpi(Ans{3}, 'y') & (length(PltModes) < 10) ;
    
    TitleTxt = [deblank(Title)  ', F = ' num2str(Freq), 'Hz'];
    
    if DoLegend
        LegendTxtReal = strcat('Real mode ', int2str(PltModes.'));
        LegendTxtImag = strcat('Imaginary mode ', int2str(PltModes.'));
    
        LegendTxt = strvcat(LegendTxtReal, LegendTxtImag);
    end
    
    figure(FigNum);
    clf;
    if exist('GUI_SetupGraphMenu.m', 'file')
        GUI_SetupGraphMenu('plot');
    end
    plot(real(Phi(:, PltModes)), Z);
    hold on;
    plot(imag(Phi(:, PltModes)), Z, ':');
    
    title(TitleTxt);
    xlabel('Mode amplitude');
    ylabel('Depth (m)');
    if DoLegend
        legend(LegendTxt);
    end
    
    view(0, -90);
end