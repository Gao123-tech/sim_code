function NewDir = PlotModes(DefaultDir)
% NewDir = PlotModes(DefaultDir)
% plots mode locations in the complex Kr plane
% Alec Duncan, June 2002
%
% Revision 0.1  21 04 2006
%               - fix plot to include re and im parts

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
        
    [ Title, Freq, Kr, z, phi, cpt, cst, rhot, deptht, cpb, csb, rhob, depthb, Nmedia, depth, rho ] = ReadModes([Path FName]);
    
    if isempty(Kr)
        uiwait(warndlg('No modes to plot'));
        return;
    end

    Ans = inputdlg({'Figure number'}, '', 1, {int2str(gcf)});
    if isempty(Ans)
       return;
    end
   
    FigNum = str2num(Ans{1});

    TitleTxt = [deblank(Title)  ', F = ' num2str(Freq), 'Hz'];

    figure(FigNum);
    clf;
    if exist('GUI_SetupGraphMenu.m', 'file')
        GUI_SetupGraphMenu('plot');
    end
    plot(real(Kr), imag(Kr), 'x');
    title(TitleTxt);
    xlabel('real(Kr) (m^-^1)');
    ylabel('imag(Kr) (m^-^1)');
    axis equal;
end