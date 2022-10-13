%Compare2TFs - script file for plotting two TransferFunctions for comparison
%            - common plots amplitude and phase stacked on top of individual impulse reponse plots

FALSE = 0;
TRUE  = 1;

% open reference TF
hdl  = helpdlg('Select Reference Transfer Function','Compare Transfer Functions');
waitfor(hdl);
Href = ReadTFFile('', 'TF', 'TF');

% open subject TFs
helpdlg({'Select Subject Transfer Functions', '<Cancel> to end selection'},'Compare Transfer Functions');
done = FALSE;
ii   = 0    ;
while ~done
    ii    = ii + 1                     ;
    H     = ReadTFFile('', 'TF', 'TF') ;
    if isempty(H)
        done = TRUE                    ;
    else
        Hsub(ii) = H                   ;
    end
end

[fhdls, current, next] = FindFigures() ;
holdon = TRUE                          ;
unrap  = FALSE                         ;
% plot graphs
ngraphs = ii - 1                       ;
fnum    = next                         ;
for ii = 1:ngraphs  
    % reference IR
    plot(ImpulseResponse(Href)    , 'auto', fnum, [4 1 3], holdon, 'b-'); 
    % subject   IR
    plot(ImpulseResponse(Hsub(ii)), 'auto', fnum, [4 1 4], holdon, 'r:'); 
    % reference and subject TF - plot last so legend will be in front
    plot([Href, Hsub(ii)], 'auto', fnum, [4 1 1], [4 1 2], holdon, unrap);
    % size
    set(fnum, 'Position', [20 20 600 900]);
    fnum = fnum + 1;
end
    
    

    


 
