function ts   = COA842_MakeTxTS(N, fc)
%COA842_MakeTxTS(N, fc)       generates source signal used in Jensen et al. Computational Ocean Acoustics section 8.4.2 (Broadband Modelling)
%USAGE      ts   = COA842_MakeTxTS(N, fc)
%
%           N    = 1024 DEFAULT
%           fc   = 50Hz DEFAULT (as in reference example)

if ~exist('N', 'var')
   N  = 1024;
end
if ~exist('fc', 'var')
   fc = 50;
end

tmax = 4/fc;
t    = [0:N-1].*tmax;
wc   = 2*pi*fc;
p    = (1-cos((wc/4).*t)) .* sin(wc.*t) ./ 2;
lstr = sprintf('COA 8.4.2 Tx signal fc = %.1fHz', fc);
ts   = TimeSeries(p, t(1), t(2), lstr);