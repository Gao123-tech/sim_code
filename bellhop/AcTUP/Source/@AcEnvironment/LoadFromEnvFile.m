function [OutEnvObj, RunInfo] = LoadFromEnvFile(EnvObj, FName)
%[EnvObj, RunInfo] = LoadFromEnvFile(EnvObj, FName)
%
%This function loads an existing environment object with information obtained from a standard
%Kraken type environment file.  Existing fields are overwritten.  
%Additional information (currently only frequency) is returned in the structure RunInfo.
%
%EnvObj - environment object
%FName - full file name (including path) of environment file
%
%If an error occurs a warning is issued and RunInfo is returned as []

Status = 1;
OutEnvObj = EnvObj;
FileOpen = 0;

%Default medium properties assumed by Kraken
Dflt.Z = 0;
Dflt.Cp = 1500;
Dflt.Cs = 0;
Dflt.Rho = 1000;
Dflt.Ap = 0;
Dflt.As = 0;

    
Fid = fopen(FName, 'rt');
Msg = [];

if Fid < 0;
    Status = 0;
    Msg = lAppendMsg(Msg, 'Couldn''t open file.');
else
    FileOpen = 1;
end

if Status
    [Vals, NRead] = ScanEnvLine(Fid, 's');   
    if NRead ~= 1
        Status = 0;
        Msg = lAppendMsg(Msg, 'Error reading title');
    else
        EnvObj.Name = Vals{1};
    end
end

if Status
    FileOpt = menu('Environment file type:', 'Kraken, KrakenC, Scooter etc.', 'Bellhop');
    [Vals, NRead] = ScanEnvLine(Fid, 'f');
    if NRead ~= 1
        Status = 0;
        Msg = lAppendMsg(Msg, 'Error reading frequency'); 
    else
        Freq = Vals{1};
        RunInfo.Freq = Freq;
    end        
end

if Status
    [Vals, NRead] = ScanEnvLine(Fid, 'f');
    if NRead ~= 1
        Status = 0;
        Msg = lAppendMsg(Msg, 'Error reading NMedia'); 
    else
        NMedia = Vals{1};
        if (NMedia < 1) | (NMedia > 20)
            Status = 0;
            Msg = lAppendMsg(Msg,  ['Illegal number of media specified: ' int2str(NMedia)]);
        end
    end        
end

if Status
    [Vals, NRead] = ScanEnvLine(Fid, 's');
    if NRead ~= 1
        Status = 0;
        Msg = lAppendMsg(Msg, 'Error reading options string'); 
    else
        Options = Vals{1};
        %Check that the specified options are supported:
        switch Options(1)
        case 'N'
            
        otherwise,
            Msg = lAppendMsg(Msg, ['Sound speed interpolation option ' Options(1) ' specified']);
            Msg = lAppendMsg(Msg, 'ActoolboxFrontend forces N option (N2-linear)');
            Msg = lAppendMsg(Msg, ' ');
        end
        
        switch Options(2)
        case 'V',
        otherwise,
            Msg = lAppendMsg(Msg, ['Top boundary condition ' Options(2) ' specified']);
            Msg = lAppendMsg(Msg, 'ActoolboxFrontend forces V option (Vacuum)');
            Msg = lAppendMsg(Msg, ' ');
        end
        
        switch Options(3)
        case 'T',
            Msg = lAppendMsg(Msg, 'Thorp attenuation specified');
            Msg = lAppendMsg(Msg, 'ActoolboxFrontend forces Thorp option for Bellhop and W option (dB per wavelength) for all other codes)');
            Msg = lAppendMsg(Msg, ' ');
            
        case {'N','F', 'M', 'W'}
            
        otherwise,
            Status = 0;
            Msg = lAppendMsg(Msg, ['attenuation option ' Options(3) ' not supported by AcToolboxFrontend']);
        end
            
    end
end

if Status
    %Some of the top boundary condition options require additional lines - these will be read but ignored (warnings issued
    %in preceding code block)
    switch Options(2)
    case {'A', 'S', 'H', 'T', 'I'},
        InLine = fgetl(Fid);
        if ~ischar(InLine)
            Status = 0;
            Msg = lAppendMsg(Msg, 'End of file encountered reading top boundary condition info.');
        end
    end
end

if Status 
    EnvObj.LayerArr = [];
    for IMedia = 1:NMedia
        LayerName = ['Layer ' int2str(IMedia)];
        
        Layer = AcLayer;
        [Layer, ThisMsg] = LoadFromEnvFile(Layer, Fid, LayerName, Dflt, Options(3), Freq);
        
        if isempty(Layer)
            Status = 0;
            Msg = lAppendMsg(Msg, ThisMsg);
            break;
        else
            EnvObj.LayerArr{IMedia} = Layer;
        end
    end
end

%Deal with the bottom boundary condition

if Status
    [Vals, NRead] = ScanEnvLine(Fid, 'sf');
    if NRead ~= 2
        Status = 0;
        Msg = lAppendMsg(Msg, 'Error reading bottom boundary condition and roughness'); 
    else
        BotOpt = Vals{1};
        LayerData.RMSRough = Vals{2};
        
        switch BotOpt(1)
        case 'A',
        otherwise,
            Status = 0;
            Msg = lAppendMsg(Msg, ['Bottom boundary option ' BotOpt(1) ' specified.']);
            Msg = lAppendMsg(Msg, 'ActoolboxFrontEnd only supports A option (acoustic halfspace)');
        end
    end
end


if Status
    BottomData = LibReadEnvFileLine(Fid, Dflt, Options(3), Freq);
    if isempty(BottomData)
        Status = 0;
        Msg = lAppendMsg(Msg, 'Error reading bottom acoustic halfspace data.');
    else
        LayerData.Name = 'Bottom halfspace';
        LayerData.Z = 0;   %Depths are referenced to top of layer
        LayerData.Cp = BottomData.Cp;
        LayerData.Cs = BottomData.Cs;
        LayerData.Rho = BottomData.Rho;
        LayerData.Ap = BottomData.Ap;
        LayerData.As = BottomData.As;
        LayerData.IsHalfSpace = 1;
        
        EnvObj.LayerArr{NMedia+1} = AcLayer(LayerData);
    end
end

 
if Status
    switch FileOpt
    case 1,
        %Kraken style file
        [Vals, NRead] = ScanEnvLine(Fid, 'ff');
        if NRead ~= 2
            Status = 0;
            Msg = lAppendMsg(Msg, 'Error reading min and max phase speeds'); 
        else
            CpMin = Vals{1};
            CpMax = Vals{2};
        end
        
        if Status
            [Vals, NRead] = ScanEnvLine(Fid, 'f');
            if NRead ~= 1
                Status = 0;
                Msg = lAppendMsg(Msg, 'Error reading max range'); 
            else
                RMax = Vals{1}*1000;
            end
        end
    end
end

if Status
    Zs = lReadListVals(Fid);
    if isempty(Zs)
        Status = 0;
        Msg = lAppendMsg(Msg, 'Error reading source depths'); 
    elseif length(Zs) > 1
        Msg = lAppendMsg(Msg, 'AcToolbox only supports a single source depth - first depth will be used'); 
        Msg = lAppendMsg(Msg, ' ');

        RunInfo.Zs = Zs(1);
    else
        RunInfo.Zs = Zs;
    end
end

if Status
    Zr = lReadListVals(Fid);
    if isempty(Zr)
        Status = 0;
        Msg = lAppendMsg(Msg, 'Error reading receiver depths'); 
    else
        RunInfo.Zr = Zr;
    end
end

if Status
    %Sort out the the receiver ranges
    switch FileOpt
    case 1,
        %Kraken style file - come up with a sensible guess based on the maximum range
        if RMax > 0
            RunInfo.RMin = RMax/100;
            RunInfo.RMax = RMax;
            RunInfo.NRange = 100;
        else
            RunInfo.RMin = [];
            RunInfo.RMax = [];
            RunInfo.NRange = [];
        end
            
    case 2,
        %Bellhop style file - read ranges directly
        RR = lReadListVals(Fid);
        if isempty(RR)
            Status = 0;
            Msg = lAppendMsg(Msg, 'Error reading receiver ranges'); 
        else
            RunInfo.RMin = min(RR)*1000;
            RunInfo.RMax = max(RR)*1000;
            RunInfo.NRange = length(RR);
        end
    end
end
        
        
%%%%%%%%%
%Tidy up
%%%%%%%%%
if Status
    OutEnvObj = EnvObj;
    Msg = lAppendMsg(Msg, 'AcToolboxFrontend calculates its own values for the following:');
    
    switch FileOpt
    case 1,
        Msg = lAppendMsg(Msg, 'NMesh, CpMin, CpMax');
    case 2,
        Msg = lAppendMsg(Msg, 'NMesh, Run type, NBeams, Alpha(1:NBeams), Step, ZBox, RBox');
    end
else
    Msg = lAppendMsg(Msg, ' ');
    Msg = lAppendMsg(Msg, '*** Environment file not loaded ***');
    
    RunInfo = [];
end

if FileOpen
    fclose(Fid);
end

if length(Msg) > 0
    uiwait(warndlg(Msg));
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function Z = lReadListVals(Fid)
Delim = [' ,' char(9)]; 

Status = 1;
[N, Count] = fscanf(Fid, '%d', 1);
if Count ~= 1
    Status = 0;
else
    Done = 0;
    ICount = 0;
    Z = zeros(1, N);
    GotSlash = 0;
    while ~Done
        InLine = fgetl(Fid);
        
        DoneLine = 0;
        while ~DoneLine
            [Str, InLine] = strtok(InLine, Delim);
            if isempty(Str)
                DoneLine = 1;
            else
                Val = str2num(Str);
                if isempty(Val)
                    DoneLine = 1;
                    if ~isempty(strfind(Str, '/'))
                        GotSlash = 1;
                        Done = 1;
                    end
                else
                    ICount = ICount + 1;
                    Z(1, ICount) = Val;
                    if ICount >= N
                        DoneLine = 1;
                        Done = 1;
                    end
                end
            end
        end
    end
    if ICount ~= N
        if GotSlash & (ICount == 2) & (N > 1)
            ZMin = Z(1);
            ZMax = Z(2);
            dZ = (ZMax-ZMin)/(N-1);
            
            Z = ZMin:dZ:ZMax;
        else
            Status = 0;
        end
    end
end

if ~Status
    Z = [];
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
function Msg = lAppendMsg(Msg, Str)
N = length(Msg);
Msg{N+1} = Str;

