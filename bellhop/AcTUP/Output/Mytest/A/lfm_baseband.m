function sigx=lfm_baseband(T_lfm, bw, sampling_rate)
%sigx=lfm_baseband(T_lfm, bw, sampling_rate) generates 
%chirp, or linear frequency modulated waveform (at the baseband)
%
%Input: 
%   T_lfm: LFM signal duration
%   bw: nominal passband signal bandwidth (fmax-fmin)
%   sampling_rate: sampling rate of the LFM
%
%Output: 
%   sigx: output LFM signal at the baseband
%
%Reference: LFM signaling and its forms
%A. Hein, Processing of SAR data: Fundamentals, Signal Processing, 
%Interferometry, Springer, New York, 2004.
%
%By Aijun Song
%Last updated on May 24, 2014
%rate of frequency sweeping
mu=bw/T_lfm;

%total samples 
ttl_samples=round(sampling_rate*T_lfm);

%time vector
t_lfm=(((0: ttl_samples-1)-ttl_samples/2)/sampling_rate).'; 

%complex linear frequency modulated signal at the baseband
sigx=exp(1j*pi*mu*t_lfm.^2);