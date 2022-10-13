function NewDir = PlotArrivals(DefaultDir)
% NewDir = PlotArrivals(DefaultDir)
% plots the amplitude delay information produced by Bellhop
% Alec Duncan, May 2002
%
% Modded 20/1/04 to correct problem if only single receiver depth specified



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

[FName, Path] = uigetfile('*.arr', 'Select file to plot');

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
    
    [ Amp, Delay, SrcAngle, RcvrAngle, NumTopBnc, NumBotBnc, NArrMat, NSD, NRD, NR, SD, RD, RR ] ...    
        = ReadArrivalsAsc([Path FName]);    
    
    if isempty(find(Amp ~= 0))
        uiwait(warndlg('No arrivals to plot'));
        return;
    end
    
    ISD = 1;  %Only plots data for first source
    
    Status = 1;
    if (NRD == 1) & (NR > 1)
        PltAgainst = 'Range';
        IRD = 1;
        
    elseif (NRD > 1) & (NR == 1)
        PltAgainst = 'Depth';
        IRR = 1;
        
    elseif (NRD > 1) & (NR > 1)
        switch(menu('Plot against - ', 'Receiver range', 'Receiver depth'))
            case 1,
                PltAgainst = 'Range';
                Ans = inputdlg({['Depth to plot (' num2str(RD(1)) 'm to' num2str(RD(end)) 'm)']}, ...
                    '', 1, {num2str(RD(1))});
                if isempty(Ans)
                    Status = 0;
                else
                    [Dum, IRD] = min(abs(str2num(Ans{1}) - RD));
                end
                
                
            case 2,
                PltAgainst = 'Depth';
                Ans = inputdlg({['Range to plot (' num2str(RR(1)) 'm to' num2str(RR(end)) 'm)']}, ...
                    '', 1, {num2str(RR(1))});
                if isempty(Ans)
                    Status = 0;
                else
                    [Dum, IRR] = min(abs(str2num(Ans{1}) - RR));
                end
                
        end
    end
end

Ans = inputdlg({'Dynamic range (dB)', 'Figure number'}, ...
    '', 1, {'60', int2str(gcf)});
if isempty(Ans)
    Status = 0;
else
    DynRange = str2num(Ans{1});
    
    FigNum = str2num(Ans{2});
end

if Status
    
    switch PltAgainst
        case 'Range'
            
            TitleTxt = [FName ', Source depth = ' num2str(SD(ISD)) 'm, Receiver depth = ' num2str(RD(IRD)) 'm'];
            PltAmp = squeeze(Amp(:, :, IRD, ISD));
            Sz = size(PltAmp);
            PltDelay = squeeze(Delay(:, :, IRD, ISD));
            PltYDat = repmat(RR, 1, Sz(2));
            PltNArr = squeeze(NArrMat(:, IRD, ISD));
            
            YTxt = 'Range (m)';
            InvertY = 0;
            
        case 'Depth'
            TitleTxt = [FName ', Source depth = ' num2str(SD(ISD)) 'm, Receiver range = ' num2str(RR(IRR)) 'm'];
            
            PltAmp = squeeze(Amp(IRR, :, :, ISD)).';
            Sz = size(PltAmp);
            PltDelay = squeeze(Delay(IRR, :, :, ISD)).';
            PltYDat = repmat(RD, 1, Sz(2));
            PltNArr = squeeze(NArrMat(IRR, :, ISD)).';
            
            YTxt = 'Depth (m)';
            InvertY = 1;
    end
    
else
    uiwait(warndlg('No arrivals to plot'));
    Status = 0;
end

if Status
    figure(FigNum);
    clf;
    if exist('GUI_SetupGraphMenu.m', 'file')
        GUI_SetupGraphMenu('plot');
    end
    PltDelayVec = reshape(PltDelay, Sz(1)*Sz(2), 1);
    PltYVec = reshape(PltYDat, Sz(1)*Sz(2), 1);
    PltAmpVec = reshape(PltAmp, Sz(1)*Sz(2), 1);
    
    Index = find((PltAmpVec == 0) | (PltDelayVec == 0));
    PltDelayVec(Index) = [];
    PltYVec(Index) = [];
    PltAmpVec(Index) = [];
    
    %stem3(PltDelayVec, PltYVec, abs(PltAmpVec));
    
    PltdB = -20*log10(abs(PltAmpVec));
    PltMin = min(PltdB);
    
    IGT = find((PltdB > PltMin+DynRange) | isnan(PltdB));
    if ~isempty(IGT)
        PltdB(IGT) = PltMin+DynRange;
    end
    
    scatter(PltDelayVec, PltYVec, 3, PltdB, 'filled');
    ThisMap = colormap;
    caxis([PltMin PltMin+DynRange]);
    colormap('default');
    Map = colormap;
    colormap(flipud(Map));  %Want small TL = big signal = red
    
    GUI_Colorbar('Transmission loss along ray (dB)');
    
    title(TitleTxt, 'Interpreter', 'none');
    xlabel('Delay (sec)');
    ylabel(YTxt);
    if InvertY
        view(0, -90);
    end
    %zlabel('Amplitude');
    
end

