function ts = MakeAsymSinusoid(alpha, T, dt, t0, N, polarity, filename)
%MakeAsymSinusoid     Constructs asymmetric sinusoidal signal with zero DC component (net zero area per period)
%                     Time series has unit maximum excursion
%
%USAGE                ts = MakeAsymSinusoid(alpha, T, dt, t0, N, polarity, filename)
%
%INPUT                alpha    = amplitude ratio (amplitude of leading halfpulse to trailing half pulse)
%                     T        = period (of asymmetric pulse)
%                     dt       = approx time step     ...            actual dt = (T/ceil(T/dt)
%                     t0       = start time
%                                =  0 DEFAULT 
%                     N        = number of periods 
%                                =  1 DEFAULT
%                     polarity = polarity of leading half pulse
%                                = +1 DEFAULT
%                                = -1    
%                     filename = path+filename destination of time series
%                              = omit or [] for no write
%                              = -1 for interactive (NOT IMPLEMENTED)
%                             
%
%OUTPUT               ts         = TimeSeries Object (CMST)
%
%THEORY
% 
%                     for asymmetric pulse ...         alpha  = T2/T1
%
%                     where T1 is the sinusoidal period of the leading  half pulse 
%                           T2 is the sinusoidal period of the trailing half pulse 
%
%                     and   T  = (T1 + T2   )    / 2  
%                              = (1  + alpha) T1 / 2
%
%                                    2T
%                     so    T1 = ------------
%                                (1  + alpha)
%                                
%
% Revision History
% ================
%
% Revision 0.0    17 February  2005 ... ALM
%
%
% Centre for Marine Science and Technology
% Physics Department
% Curtin University
% Perth, Western Australia                           

% intialisations
TRUE   = 1     ;
FALSE  = 0     ;
TWOpi  = 2*pi  ;
ask    = FALSE ;

% calc periods
T1 = 2*T/(1+alpha);
T2 = 2*T - T1     ;

% calc amplitudes with unit maximum excursion for pulsetrain
if alpha >= 1
    a1 = 1      ;
    a2 = 1/alpha;
else
    a1 = alpha  ;
    a2 = 1      ;
end
a1 = a1 * polarity      ;
a2 = a2 * polarity * -1 ;


% time step
dt = T/ceil(T/dt);

% create time base
t    = [0:dt:T]'  ;

% create weight function 
zeroidx    = max(find(t <= T1/2));
lotrans    = zeroidx - 0         ;
hitrans    = zeroidx + 1         ;
n          = length(t)           ;
% allow for 10point overlap
weight                      = [ones(size([1:zeroidx]'));zeros(size([zeroidx+1:n]'))] ;
%split timebase
idx1       = [1:hitrans]' ;
idx2       = [lotrans:n]' ;
t1         = t(idx1)      ;
t2         = t(idx2)      ;
t20        = t2(1)        ;

% generate time series ordinate vector
v1        = zeros(size(t))                  ;
v2        = v1                              ;
TWOpi_T1  = TWOpi/T1                        ;
TWOpi_T2  = TWOpi/T2                        ;
v1(idx1)  = a1.*sin(TWOpi_T1 .* t1)         ;
v2(idx2)  = a2.*sin(TWOpi_T2 .* (t2-t20))   ;
vector    = weight.*v1 + (1-weight).*v2     ;                         

% create timeseries
ts = cTimeSeries(vector, t0, dt, '');



% SAVE cTimeSeries 
% check input and action
ask = FALSE;
if     ~exist('filename', 'var')
elseif isempty(filename)
elseif isnumeric(filename)
    % use file ID directly if ever implemented
    % ...

    % now test for uigetfile
    if filenmame < 0
        ask = TRUE;
    end
elseif ischar(filename)
    WriteFile(ts, filename);
end
% interactive option
if ask 
    [filename, pathname] = uiputfile('*.ts', 'Write to cTimeSeries file ...');
    if filename ~= 0 
        filename = [pathname filename];
        WriteFile(ts, filename);
    end    
end






