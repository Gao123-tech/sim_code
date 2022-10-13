function [y, adj_ir_vec]=uw_isi(ir_vec, sl_db, tx_source, nv_db)
%y=uw_isi(ir_vec, sl_db, tx_source, nv) simulate underwater ISI channel
%effects from multipath and ambient noisee
%
%Input: 
%   ir_vec is the channel impulse response (in baseband)    
%   sl_db is the source level in dB
%   tx_source is the source signal (in baseband)
%   nv_db is the noise level in dB
%
%Output: 
%   y is the channel output
%   adj_ir_vec is the source level adjusted impulse response
%
%By Aijun Song
%Last updated on May 24, 2014
%
%adjust the impulse response based source level
adj_ir_vec=ir_vec*10^(sl_db/20);

%channel effects (multipath)
tx_sig=conv(tx_source, adj_ir_vec);

%convert noise variance in dB to the linear scale
namp=10^(nv_db/20);

%complex-number noise
noise=namp*(0.707*randn(size(tx_sig))+1j*0.707*randn(size(tx_sig)));

% rho1=10*log10(sum(abs(amp_vec(end)).^2)/namp^2)
rho2=10*log10(mean(abs(tx_sig).^2)/mean(abs(noise).^2));


%channel output
y=tx_sig+noise;

% noise_variance=10*log10(mean(abs(noise).^2));
% disp(['Generated noise variance=' num2str(noise_variance) 'dB. Specified noise variance=' num2str(nv_db)]);
