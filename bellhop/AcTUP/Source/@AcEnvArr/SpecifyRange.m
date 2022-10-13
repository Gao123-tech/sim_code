function Range = SpecifyRange(Obj, Range)

Ans = inputdlg({'Horizontal range for this environment (m)'}, '', 1, {num2str(Range)});
if ~isempty(Ans)
    Range = str2num(Ans{1});
end