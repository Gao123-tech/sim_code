%Compare2TFs - script file for plotting two TransferFunctions for comparison
%            - common plots amplitude and phase stacked on top of individual impulse reponse plots

FALSE = 0;
TRUE  = 1;

% graph setup
IRylim = [-0.000025, 0.000025];
IRxlim = [0 3];
autoy  = FALSE;
xr     = 1000;
% eigenray arrival markers
aplot.hold    = 1  ;
aplot.marker  = '|';
aplot.m_col   = 'k';
aplot.m_y     = 0.15*max(IRylim);
aplot.lab_col = 'k';

% setup lag - there's enough resolution to go from t=0;
lag = 0;

% open reference TF
hdl  = helpdlg('Select Reference Transfer Function','Compare Transfer Functions');
set(hdl, 'WindowStyle', 'Modal');
waitfor(hdl);
Href = ReadTFFile('', 'TF', 'TF')     ;
if isempty(Href), return;          end;
Href = set(Href, 'label', 'RAMGeo')   ;
Href = set(Href, 'lag'  , 0       )   ;

% open subject TFs
hdl = helpdlg({'Select Subject Transfer Functions', '<Cancel> to end selection'},'Compare Transfer Functions');
set(hdl, 'WindowStyle', 'Modal');
waitfor(hdl);
done = FALSE;
ii   = 0    ;
while ~done
    ii    = ii + 1                     ;
    H     = ReadTFFile('', 'TF', 'TF') ;
    if isempty(H)
        done = TRUE                    ;
    else
        H         = set(H, 'lag', 0)   ;
        Hsub(ii)  = set(H, 'label', ['Bellhop ', get(H, 'label')])   ;
    end
end

[fhdls, current, next] = FindFigures() ;
IR     = ImpulseResponse(Href)         ;
holdon = TRUE                          ;
unrap  = FALSE                         ;
% plot graphs
ngraphs = ii - 1                       ;
fnum    = next                         ;
for ii = 1:ngraphs  
    aplot.fnum    = fnum;
    % reference IR
    
    
    plot(IR , 'auto', fnum, [5 1 3], holdon, 'b-'); 
    title('Impulse Responses');
    xlabel('');
    if autoy
        ylim3 = get(gca,'YLim');
    else
        set(gca, 'YLim', IRylim);
        ylim3 = IRylim;
    end
    % subject   IR
    IRs = ImpulseResponse(Hsub(ii));
    plot(IRs, 'auto', fnum, [5 1 4], holdon, 'r:'); 
    title('');
    if autoy
        ylim4 = get(gca,'YLim');
    else
        set(gca, 'YLim', IRylim);
        ylim4 = IRylim;
    end
    if autoy
        % make y limits the same
        if ylim3(1) - ylim4(1) ~= 0 | ylim3(2) - ylim4(2) ~= 0
            ylim = [min(ylim3(1),ylim4(1)), max(ylim3(2),ylim4(2))];
            subplot(5,1,3);
            set(gca, 'YLim', ylim);
            subplot(5,1,4);
            set(gca, 'YLim', ylim);
            ylim3 = ylim;
            ylim4 = ylim;
        end
    end
    % plot eigenray arrivals
    aplot.sub       = [5 1 3];
    aplot.lab_y     = [0.5, 0.75]*max(ylim3);
    [data, labels]  = FindEigenRaysSimple(700, 900, xr, 2000, 1500, 10, [], aplot);
    aplot.sub       = [5 1 4];
    aplot.lab_y     = [0.5, 0.75]*max(ylim4);
    [data, labels]  = FindEigenRaysSimple(700, 900, xr, 2000, 1500, 10, [], aplot);


    % cf  IR - for close up
    plot(IR , 'auto', fnum, [5 1 5], holdon, 'b-'); 
    plot(IRs, 'auto', fnum, [5 1 5], holdon, 'r:'); 
    if autoy
        set(gca, 'XLim', IRxlim);
        ylim = get(gca, 'YLim');
    else
        set(gca, 'XLim', IRxlim, 'YLim', IRylim);
        ylim = IRylim;
    end
    % plot eigenray arrivals
    aplot.fnum    = fnum;
    aplot.sub     = [5 1 5];
    aplot.lab_y   = [0.5, 0.75]*max(ylim);
    [data, labels]  = FindEigenRaysSimple(700, 900, xr, 2000, 1500, 10, [], aplot);
    title('');
    % reference and subject TF - plot last so legend will be in front
    plot([Href, Hsub(ii)], 'auto', fnum, [5 1 1], [5 1 2], holdon, unrap, {'b-','r:'});
    subplot(5,1,1);
    title('Transfer Functions');
    % size
    set(fnum, 'Position', [20 20 1000 900]);
    fnum = fnum + 1;
    % plot comparison also as seperate graph
    % cf  IR - for close up
    plot(IR , 'auto', fnum, [1 1 1], holdon, 'b-'); 
    plot(IRs, 'auto', fnum, [1 1 1], holdon, 'r:'); 
    if autoy
        set(gca, 'XLim', IRxlim);
        ylim = get(gca, 'YLim');
    else
        set(gca, 'XLim', IRxlim, 'YLim', IRylim);
        ylim = IRylim;
    end
    % plot eigenray arrivals
    aplot.fnum    = fnum;
    aplot.sub     = [1 1 1];
    aplot.lab_y   = [0.5, 0.75]*max(ylim);
    [data, labels]  = FindEigenRaysSimple(700, 900, xr, 2000, 1500, 10, [], aplot);
    title('');
    fnum = fnum + 1;
    
end
    
    

    


 
