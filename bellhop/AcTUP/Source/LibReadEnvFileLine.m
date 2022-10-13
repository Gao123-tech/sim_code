function  EnvData = LibReadEnvFileLine(Fid, Dflt, AttenOpt, Freq)
%EnvData = LibReadEnvFileLine(Fid, Dflt, AttenOpt)
%
%Reads one line of sound speed data from a Kraken environment file
%Fid is the file ID of the input file
%Dflt contains default values for the sound speeds, density, and attenuations
%AttenOpt specifies the expected format of the attenuation data.  This will be converted to dB/wavelength.
%
%EnvData is empty if an error occurs


EnvData = Dflt;
Done = 0;
[Vals, NRead] = ScanEnvLine(Fid, 'ffffff');   
if NRead < 1
    Done = 1;
    EnvData = [];
end

if ~Done
    EnvData.Z = Vals{1};
    if NRead >= 2
        EnvData.Cp = Vals{2};
    end
    
    if NRead >= 3
        EnvData.Cs = Vals{3};
    end
    
    if NRead >= 4
        EnvData.Rho = Vals{4} * 1000;
    end
    
    if NRead >= 5
        EnvData.Ap = LibConvertAttenuation(Vals{5}, AttenOpt, 'W', EnvData.Cp, Freq);        
    end
    
    if NRead >= 6
        EnvData.As = LibConvertAttenuation(Vals{6}, AttenOpt, 'W', EnvData.Cp, Freq);        
    end
end
    


    
        
    
    