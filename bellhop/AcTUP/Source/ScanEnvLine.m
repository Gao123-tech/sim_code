function [Vals, NRead] = ScanEnvLine(Fid, Fmt)
%[Vals, NRead] = ScanEnvLine(Fid, Fmt)
%
% Reads a line of data from a Kraken environment file and parses it for numeric and string values
% as specified. Remainder of line is discarded.
%
%Fid - file ID of input file
%Fmt - format string.  This is a simplified format string which
%      specifies the format of the data to be read.  Two characters are recognised:
%      f - numeric value.  May be delimited by spaces, commas or tabs.
%      s - string.  Strings are assumed delimityed by ' characters.
%      eg. 'sfff'  would read one string and three numeric values
%
%Vals - a cell array containing the values and strings read
%NRead - the number of values and strings successfully read and converted

NumDelim = [' ,' char(9)];  %String containing possible delimters for numerics
StrDelim = '''';            %Delimiter for string

NRead = 0;
Vals = [];

Status = 1;

InLine = fgetl(Fid);
if ~ischar(InLine)
    Status = 0;
end

Done = 0;
ISub = 1;
while ~Done & Status
    if ISub > length(Fmt)
        Done = 1;
    else
        switch Fmt(ISub)
        case 'f'
            [Str, InLine] = strtok(InLine, NumDelim);
            if isempty(Str)
                Done = 1;
            else
                ThisVal = str2num(Str);
                if isempty(ThisVal)
                    Done = 1;
                else
                    Vals{ISub} = ThisVal;
                    ISub = ISub + 1;
                    NRead = NRead + 1;
                end
            end
        case 's'
            IDelim = findstr(StrDelim, InLine);
            if (length(IDelim) < 2)
                Done = 1;
            else
                if (IDelim(2) - IDelim(1))< 2
                    String = '';
                else
                    String = InLine(IDelim(1)+1:IDelim(2)-1);
                end
                Vals{ISub} = String;
                ISub = ISub + 1;
                NRead = NRead + 1;
                
                if IDelim(2) < length(InLine)
                    InLine = InLine(IDelim(2)+1:end);
                end
            end
        end
    end
end

