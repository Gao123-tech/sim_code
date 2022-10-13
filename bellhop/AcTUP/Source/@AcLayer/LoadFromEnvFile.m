function [Layer, Msg] = LoadFromEnvFile(Layer, Fid, LayerName, Dflt, AttenOpt, Freq)
%        [Layer, Msg] = LoadFromEnvFile(Layer, Fid, LayerName, Dflt, AttenOpt, Freq);
%
%Loads acoustic layer object with information read from a standard kraken environment file
%Fid - file identifier
%LayerName - layer name
%Dflt - structure containing default parameter values for first line of layer
%AttenOpt - character specifying units attenuation is in
%Freq - frequency (Hz) 
%
%If successful a layer object is returned
%If unsuccessful OutLayer is [] and a string containing an error message is returned in Msg



Status = 1;

[Vals, NRead] = ScanEnvLine(Fid, 'fff');   
if NRead ~= 3
    Status = 0;
    Msg = ['Error reading first line of layer information for layer: ' LayerName];
end

if Status
    NMesh = Vals{1};
    Layer.RMSRough = Vals{2};
    ZLast = Vals{3};
    
    Layer.Name = LayerName;
    Layer.IsHalfSpace = 0;
    FirstLine = 1;
    Z = [];
    Cp = [];
    Cs = [];
    Rho = [];
    Ap = [];
    As = [];    
end

Done = 0;

while ~Done & Status
    EnvData = LibReadEnvFileLine(Fid, Dflt, AttenOpt, Freq);
    
    if isempty(EnvData)
        Status = 0;
        Msg = ['Error reading sound speed data for layer: ' LayerName];
    else
        if FirstLine
            ZTop = EnvData.Z;
            FirstLine = 0;
        end
        Z = [Z, EnvData.Z-ZTop];
        Cp = [Cp, EnvData.Cp];
        Cs = [Cs, EnvData.Cs];
        Rho = [Rho, EnvData.Rho];
        Ap = [Ap, EnvData.Ap];
        As = [As, EnvData.As];
        
        if EnvData.Z >= ZLast
            Done = 1;
        else
            Dflt = EnvData;  %Current line is default for next line
        end
    end
end

if Status
    Layer.Z = Z;
    Layer.Cp = Cp;
    Layer.Cs = Cs;
    Layer.Rho = Rho;
    Layer.Ap = Ap;
    Layer.As = As;
    Msg = '';
else
    Layer = [];
end
    

