function NewDir = PlotReflnCoefft(DfltDir)

NewDir = DfltDir;
Here = pwd;

if nargin >= 1
   DirStat = 1;
   if ~exist(DfltDir, 'dir')
       DirStat = LibMakeDirectory(DfltDir);
   end
   if DirStat
       cd(DfltDir);
   end
end
[File, Path] = uigetfile('*.bty', 'Bathymetry file');
cd(Here);

if File ~= 0
    NewDir = Path;
    InFile = fopen([Path File], 'rt');
    NPt = fscanf(InFile, '%f', 1);
    InDat = fscanf(InFile, '%f', [2, NPt]);
    
    Range = InDat(1, :)*1000;
    Depth = InDat(2, :);
    
    Ans = inputdlg({...
   		      'Figure number', ...
      		   'Hold current plot (y/n)', ...
         	   'Line style'}, '', 1, {int2str(gcf), 'n', 'b-'});
       
	if ~isempty(Ans)  
   		FigNum = str2num(Ans{1});
   		HoldCurrent = strcmpi(Ans{2}, 'y');
	   	Style = Ans{3};
        
        figure(FigNum);
        if ~HoldCurrent
            clf;
            if exist('GUI_SetupGraphMenu.m', 'file')
                GUI_SetupGraphMenu('plot');
            end
        end
        hold on;
        plot(Range, Depth, Style);
        
        if ~HoldCurrent
            ylabel('Depth (m)');
            title(['Bathymetry - ' File], 'Interpreter', 'none');
            xlabel('Range (m)');
            view(0, -90);
        end
    end
    fclose(InFile);
end

