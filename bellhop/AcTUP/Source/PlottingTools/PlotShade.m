function NewDir = PlotShade(DefaultDir)
% PlotShade()
% plots the shade file
%Original by Mike Porter,
%Modded by Alec Duncan
%Modded by Alec Duncan to plot binary shade files 13/6/02
%Modded by Alec Duncan to support files containing data from multiple frequencies 6/9/02
%
%Revision 3.1   11 April 2004 ... ALM
%               - User safe figure number default



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

[FName, Path] = uigetfile('*.shd', 'Select file to plot');

if ChangeDir
   cd(StartDir);
end

Status = 1;
if FName == 0
    if nargin < 1
        NewDir = pwd;
    else
        NewDir = DefaultDir;
    end
    Status = 0;
end

if Status
   NewDir = Path;
   filename = [Path FName];
   
   Fid = fopen( filename, 'rb' );
   if Fid < 0
       uiwait(warndlg(['Couldn''t open input file: ' filename]));
       Status = 0;
   end
end

if Status
	% Pre-read the file to determine what frequencies are present
    FVec = [];
    FileStatus = 1;
    while FileStatus
	    [ pltitl, freq, nsd, nrd, nrr, sd, rd, rr, tlt, FileStatus ] = ReadShadeBin( Fid );
        if FileStatus
            FVec = [FVec freq];
        end
    end
    if isempty(FVec)
        Status = 0;
    else
        if length(FVec) > 1
            if length(FVec) < 20
                Prompt = {['Choose frequency to plot: ' num2str(FVec)]};
            else
                Prompt = {['Choose frequency to plot: ' num2str(FVec(1)) ' to ' num2str(FVec(end))]};
            end
            Ans = inputdlg(Prompt, '', 1, {num2str(FVec(1))});
            
            if isempty(Ans)
                Status = 0;
            else
                ThisFreq = str2num(Ans{1});
                [Dummy, IFreq] = min(abs(FVec - ThisFreq));
            end
        else
            IFreq = 1;
        end
        if Status
            frewind(Fid);
                
            for ISub = 1:IFreq
           	    [ pltitl, freq, nsd, nrd, nrr, sd, rd, rr, tlt, Status ] = ReadShadeBin( Fid );
            end
        end
    end
    fclose(Fid);
end

if Status
	isd = 1;  %index of source to be plotted
   if nrd <= 1
      msgbox(...
         'Propagation code must be run with multiple receiver depths in order for field to be plotted', ...
      'PlotShade:', 'warn');
      return;
   end
   
	zt = rd;
	taker = 1:nrr;
	rt = rr( taker );


    %Ans = inputdlg({'Figure number', 'Plot dynamic range (dB)', ...
    %        'Remove cylindrical spreading (y/n)'}, '', 1, {int2str(gcf), '60', 'n'});
    
    [fhdls, current, next] = FindFigures();
    Ans = inputdlg({'Figure number', 'Plot dynamic range (dB)', ...
            'Remove cylindrical spreading (y/n)'}, '', 1, {int2str(next), '60', 'n'});
    if isempty(Ans)
       return;
    end
   
    FigNum = str2num(Ans{1});
    DynRange = str2num(Ans{2});
    RemoveCylindrical = strcmpi(Ans{3}, 'y');
   
    figure(FigNum);
    clf;
    if exist('GUI_SetupGraphMenu.m', 'file')
        GUI_SetupGraphMenu('pcolor');
    end
    
    TL = -20*log10(abs(tlt));
    if RemoveCylindrical
	    TL = TL - ones( nrd, 1 ) * 10.0 * log10( rt )';
    end
    
    PltTLMin = floor(min(min(TL))/5)*5;  %Minimum transmission loss for plot - round to nearest 5 dB
    PltTLMax = PltTLMin + DynRange;

	pcolor( rt, zt, TL ); 
    shading interp;
    caxis([PltTLMin PltTLMax]);
    colormap('default');
	Map = colormap;
	colormap(flipud(Map));  %Want small TL = big signal = red

	h = colorbar;
    h2 = get(h, 'ylabel');
    if RemoveCylindrical
        set(h2, 'string', 'Transmission Loss - 10logR (dB)');
    else
        set(h2, 'string', 'Transmission Loss (dB)');
    end
	view( 0, -90 );
	xlabel( 'Range (m)' ); ylabel( 'Depth (m)' );
    title( [deblank( pltitl ) '. f=' num2str(freq) 'Hz, Zs=' num2str(sd(isd)) 'm.'] , ...
      'interpreter', 'none');  
end
