function ts = COA842_MakeTxTS(N, fc, n)
%COA842_MakeTxTS(N, fc)       generates source signal used in Jensen et al. Computational Ocean Acoustics section 8.4.2 (Broadband Modelling)
%USAGE      ts   = COA842_MakeTxTS(N, fc)
%
%           N    = number of points in signal (interesting bit ... signal padded with zeros to 4N total length) - DEFAULT = 1024
%           fc   = centre frequency            - DEFAULT/REF = 50Hz
%           n    = number of periods in signal - DEFAULT/REF = 4

if ~exist('N' , 'var')
   N  = 1024;
end
if ~exist('fc', 'var')
   fc = 50;
end
if ~exist('n' , 'var')
   n =4;
end

tmax = n/fc;
t    = [0:N-1]'.*tmax./N;
wc   = 2*pi*fc;
p    = (1-cos((wc/n).*t)) .* sin(wc.*t) ./ 2;
% pad to 4x length p
pp   = zeros([4*N, 1]);
pp(1:N) = p;
lstr = sprintf('COA 8.4.2 Tx signal fc = %.1fHz', fc);
ts   = cTimeSeries(pp, t(1), t(2), lstr);