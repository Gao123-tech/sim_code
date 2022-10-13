function [npsd_db]=ambientnoise_psd(windspeed, fc)
%[npsd_db]=ambientnoise_psd(windspeed, fc) calculates ocean ambient noise 
%power spectrum density for center frequecy fc (in hertz).
%
%Input: 
%   windspeed: wind speed for ambient noise level calculation
%   fc: center frequency of the acoustic band
%
%Output: 
%   npsd_db: noise power spectrum density in dB
%
%Ref: On the Relationship Between Capacity and Distance in an Underwater 
%Acoustic Communication Channel, M. Stojanovic, 
%WUWNet?6, September 25, 2006, Los Angeles, CA. Page 3, Eq (6).
%
%Aijun Song
%Last updated on May 16, 2014
%Turbulance noiseÕƒ¡˜‘Î…˘
ANturb_dB=17-30*log10(fc/1000);

%Ambient noise in dB (wind driven noise)
ANwind_dB=50+7.5*sqrt(windspeed)+20*log10(fc/1000)-40*log10(fc/1000+0.4);

%Thermo noise in dB (wind driven noise)»»‘Î…˘
ANthermo_dB=-15+20*log10(fc/1000);

%Total noise PSD
npsd_db=10*log10(10^(ANturb_dB/10)+10^(ANwind_dB/10)+10^(ANthermo_dB/10));
% %Noise variance=bandwidth*npsd_db, assuming a flat spectrum within the band.
% nv=npsd_db+10*log10(bandwidth);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%End of file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%