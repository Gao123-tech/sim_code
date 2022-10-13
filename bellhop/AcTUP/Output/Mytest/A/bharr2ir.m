function ir_vec=bharr2ir(delay, amp, sampling_rate)
%ir_vec=bharr2ir(delay, amp, sampling_rate) converts BELLHOP arrival-time
%outputs to impulse responses.
%
%Input:
%   delay, amp: BELLHOP arrival delay and amplitude (complex) 
%   sampling_rate: sampling rate of the impulse response
%
%Output:
%   ir_vec: impulse responses at the baseband, sampled at the sampling rate
%   specified
%
%By Aijun Song
%Last updated on May 24, 2014
%remove arrivals with zero delays
valid_delay_index=find(delay>0);
if isempty(valid_delay_index)
    disp('[bharr2ir]Error: Zero path simulated by BELLHOP.');
    return;
end
delay_vec=delay(valid_delay_index);
amp_vec=amp(valid_delay_index);

%covert BELLHOP arr to an impulse response
delay_min=min(delay_vec);
delay_max=max(delay_vec);

cir_length=round(delay_max*sampling_rate);
ir_vec=zeros(cir_length, 1);

%find individual ray paths
for icn=1: length(delay_vec)
    %calculate the arrival index
    %init_delay gives some zeros priror to the first path
    arr_id=round(delay_vec(icn)*sampling_rate);
    
    %generate impulse response. Note that sometime, multiple returns can be
    %generate for the same delay in BELLHOP. 
    ir_vec(arr_id)=ir_vec(arr_id)+amp_vec(icn);
end