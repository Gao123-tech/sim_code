function AttOut = LibConvertAttenuation(AttIn, AttenOptIn, AttenOptOut, C, Freq)
%AttOut = LibConvertAttenuation(AttIn, AttenOptIn, AttenOptOut, C, Freq)
%
%Converts between possible units for attenuation
%AttIn - input attenuation value
%AttenOptIn - character specifying units of input attenuation (same as for kraken environment files).  Can be:
% N - Nepers/m
% F - dB/(kmHz)
% M - dB/m
% W - dB per wavelength
%AttenOptOut - character specifying units output is required in (same possibilities as AttenOptIn)
%C - sound speed (m/s)
%Freq - frequency (Hz)
%
%Returns [] if input or output options are unknown
%

%First convert all other units to nepers per meter
switch AttenOptIn
case 'N',
    Att = AttIn;
case 'F'
    Att = AttIn * Freq / 8686;
case 'M'
    Att = AttIn / 8.686;
case 'W';
    Lambda = C/Freq;
    if Lambda == 0
        Att = 0;
    else
        Att = AttIn / (Lambda * 8.686);
    end
otherwise
    Att = [];
end

if isempty(Att)
    AttOut = [];
else
    switch(AttenOptOut)
    case 'N',
        AttOut = Att;
    case 'F'
        AttOut = Att * 8686 / Freq;
    case 'M'
        AttOut = Att * 8.686;
    case 'W';
        Lambda = C/Freq;
        AttOut = Att * (Lambda * 8.686);
    otherwise
        AttOut = [];
    end
end        