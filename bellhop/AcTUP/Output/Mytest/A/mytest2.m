clear
clc
filename = 'Test_00100.arr';
sampling_rate=20000;
[ amp1, delay1, SrcAngle, RcvrAngle, NumTopBnc, NumBotBnc, narrmat, Pos ]... 
 = read_arrivals_asc(  filename );
amp=amp1(50,:);
delay=delay1(50,:);
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





