function est_ir_vec=lfm_mf(rx_signal, tx_source, cir_length)
%est_ir_vec=lfm_mf(tx_source, cir_length) performs matched-filtering
%operations for linear frequency modulated transmission.
%
%Input: 
%   tx_source: the source signal (LFM baseband)
%   cir_length: the intended channel length
%
%Output:
%   est_ir_vec: estimated impulse response at the baseband
%
%Reference: Auto-correlation of LFM signals
%A. Hein, Processing of SAR data: Fundamentals, Signal Processing, 
%Interferometry, Springer, New York, 2004.
%
%By Aijun Song
%Last updated on May 24, 2014
%total samples in the source signal
ttl_samples=length(tx_source);

%generate matched-filter from the source signal
txsig_flip=conj(tx_source(end:-1:1));

%matched-filtering
%division by ttl_samples is necessary for normalization
est_ir_vec=conv(txsig_flip, rx_signal)/ttl_samples;
est_ir_vec=est_ir_vec(length(txsig_flip): length(txsig_flip)+cir_length-1);