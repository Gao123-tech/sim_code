function NewDir = PlotReflnCoefft(DfltDir)
%Revision 0       ~ 2002
%
%Revision 0.1     27 July 2005 - ALM
%                 - add option to plot as bottom loss (transmission coefficient in dB) vs grazing (radians) for DSTO
%Revision 0.2     27 July 2005 - ALM
%                 - add bottom loss gradient plot (for quick inversion checks with DSTO data)

persistent LastPath;

Here = pwd;
NewDir = Here;
if nargin >= 1
   DirStat = 1;
   if ~exist(DfltDir, 'dir')
       DirStat = LibMakeDirectory(DfltDir);
   end
   if DirStat
       cd(DfltDir);
       NewDir = DfltDir;
   end
end
if ~isempty(LastPath) 
   if LastPath ~= 0
      if exist(LastPath, 'dir')
         cd(LastPath);
      end
   end
end

[File, Path] = uigetfile('*.brc', 'Reflection coefft file');
cd(Here);
LastPath = Path;

if File ~= 0
    NewDir = Path;
    InFile = fopen([Path File], 'rt');
    NPt    = fscanf(InFile, '%f', 1);
    InDat  = fscanf(InFile, '%f', [3, NPt]);
    
    Theta = InDat(1, :);
    Mag   = InDat(2, :);
    Phase = mod(InDat(3, :)+180, 360)-180;
    
    Ans   = inputdlg({...
   		            'Figure number', ...
      	            'Hold current plot (y/n)', ...
         	         'Line style'}, '', 1, {int2str(gcf), 'n', 'b-'});
            
    PlotOpt = menu('Plotting options:', ...
                   'R   (#) vs grazing(°)', ...
                   'BL (dB) vs grazing (rad)', ...
                   'dBL/dtheta (dB/rad) vs grazing (rad)', ...
                   'BL and BL/Theta' );
                
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
        switch PlotOpt
           case 1
              subplot(2,1,1);
              hold on;
              plot(Theta, Mag, Style);
              ylabel('Magnitude');
              title(['Bottom reflection coefficient - ' File], 'Interpreter', 'none');
              subplot(2,1,2);
              hold on;
              plot(Theta, Phase, Style);
              ylabel('Phase (deg)');
              xlabel('Grazing angle (deg)');
           case 2
              hold on;
              Theta_rad = Theta .* (pi / 180);
              BL_dB     = -20.*log10(Mag);
              plot(Theta_rad, BL_dB, Style);
              ylabel('bottom loss  (dB)');
              xlabel('grazing angle (rad)');              
              title(['Bottom Loss - ' File], 'Interpreter', 'none');              
            case 3
              Theta_rad = Theta .* (pi / 180);
              dTheta    = Theta_rad(2) - Theta_rad(1);
              BL_dB     = -20.*log10(Mag);
              dBdTheta  = diff(BL_dB)/dTheta
              plot(Theta_rad(2:end), dBdTheta, Style);
              ylabel('bottom loss gradient (dB/rad)');
              xlabel('grazing angle (rad)');  
              title(['Bottom Loss Gradient - ' File], 'Interpreter', 'none');
            case 4
              subplot(2,1,1);
              hold on;
              Theta_rad = Theta .* (pi / 180);
              BL_dB     = -20.*log10(Mag);
              plot(Theta_rad, BL_dB, Style);
              ylabel('bottom loss  (dB)');
              xlabel('grazing angle (rad)');              
              title(['Bottom Loss - ' File], 'Interpreter', 'none');              
              subplot(2,1,2);
              hold on;
              BL_Theta  = BL_dB./Theta_rad;
              plot(Theta_rad, BL_Theta, Style);
              ylabel('bottom loss/\theta (dB/rad)');
              xlabel('grazing angle (rad)');  
       end
   end
   fclose(InFile);
end

