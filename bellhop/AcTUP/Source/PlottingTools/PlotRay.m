function NewDir = PlotRay( DefaultDir )
%Original by Mike Porter,
%Modded by Alec Duncan

FileFormat = 2;   %1 for original file format, 2 for 24/6/2002 and later

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

[FName, Path] = uigetfile('*.ray', 'Select file to plot');

if ChangeDir
   cd(StartDir);
end

if FName == 0
   NewDir = DefaultDir;
else
   
   NewDir = Path;
   filename = [Path FName];
   
	% plots a BELLHOP ray file

	% open the file

	fid = fopen( filename, 'r' );

	% read header stuff

	TITLE  = fgetl(  fid );
	FREQ   = fscanf( fid, '%f', 1 );
	NBEAMS = fscanf( fid, '%i', 1 );
	DEPTHT = fscanf( fid, '%f', 1 );
	DEPTHB = fscanf( fid, '%f', 1 );

    
	% read rays
    Ans = inputdlg({...
   		      'Figure number', ...
      		   'Hold current plot (y/n)', ...
         	   'Line style'}, '', 1, {int2str(gcf), 'n', 'g'});
	if isempty(Ans)
        return;
	end
   
   	FigNum = str2num(Ans{1});
   	HoldCurrent = strcmpi(Ans{2}, 'y');
	Style = Ans{3};
   
	figure(FigNum);
    if ~HoldCurrent
    	clf;
        if exist('GUI_SetupGraphMenu.m', 'file')
            GUI_SetupGraphMenu('plot');
        end
	    view( 0, -90 ); % flip plot so that z-axis is pointing down
   	    xlabel( 'Range (m)' )
	    ylabel( 'Depth (m)' )
        NTitle = length(TITLE);
        for IChar = 1:NTitle
            if TITLE(IChar) == ''''
                TITLE(IChar) = ' ';
            end
        end
        title( [deblank( TITLE ) '. f=' num2str(FREQ) 'Hz'] , ...
            'interpreter', 'none');  
    end
	hold on

	for ibeam = 1:NBEAMS
        if FileFormat >= 2;
    	    LaunchAngle = fscanf( fid, '%f', 1 );
	        if isempty( LaunchAngle ) 
               break; 
            end
        end
        
   	    nsteps = fscanf( fid, '%i', 1 );
	    if isempty( nsteps ) 
            break; 
        end
        
        if FileFormat >= 2
            NBounce1 = fscanf( fid, '%i', 1 );
            NBounce2 = fscanf( fid, '%i', 1 );
        end
            
   	    ray = fscanf( fid, '%f', [2 nsteps] );
        plot( ray( 1, : ), ray( 2, : ), Style )
	end	% next beam

	fclose( fid );
end




