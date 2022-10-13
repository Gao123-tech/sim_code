%lfm_sounding performs sounding the underwater acoustic communication
%channel through the use of linear frequency modulated (LFM) signals. 
%The simulation uses output of BELLHOP arrival-time run 
%to construct the impulse response. Over
%the BELLHOP channel and an ambient noise model, 
%the LFM signal is transmitted and received. Then the program uses
%matched-filtering of the LFM to estimate impulse responses.
%The technique is commonly referred to as pulse compression. 
%The simulation is performed at the baseband. 
%
%References: 
%1) Ambient noise model: 
%Capacity and Distance in an Underwater Acoustic Communication Channel, 
%M. Stojanovic, WUWNet?6, September 25, 2006, Los Angeles, CA.
%
%2) Pulse compression with linear frequency modulated (LFM) signal
%A. Hein, Processing of SAR data: Fundamentals, Signal Processing, 
%Interferometry, Springer, New York, 2004.
%
%
%by Aijun Song
%Last updated on May 24, 2014
clear; close all; clc; 

%sampling rate of the simulation
sampling_rate=400000;

%acoustic center frequency, it should be consistent with BELLHOP
%calculation. It also determines the ambient noise level.
fc=25000;

%source signal specs
sl_db=172;  %soruce power dB level
T_lfm=0.04096; %chirp signal duration in seconds
bw=5000;    %chirp signal bandwidth

%BELLHOP run ID
env_id='Test_20000';

%wind-driven noise specs
windspeed=5;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Step 1: Source signal generation (at the baseband)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%create source waveform: LFM at the baseband
tx_source=lfm_baseband(T_lfm, bw, sampling_rate);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Step 2: Propagation through the underwater channel: Multipath and noise
%effects
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Narrmx=10; %limit ourselves to use the first Narrmax paths
%read BELLHOP arr file:
[ amp1, delay1, SrcAngle, RcvrAngle, NumTopBnc, NumBotBnc, narrmat, Pos ] ...
    = read_arrivals_asc( [env_id '.arr'] ) ;
[m,n]=size(amp1);
amp=amp1(m,:);
delay=delay1(m,:);
%convert BELLHOP output to impulse response
ir_vec=bharr2ir(delay, amp, sampling_rate);
%strongest tap in dB
maxamp_db=20*log10(max(abs(amp)))+sl_db;%声源级（dB）加由于信道衰减的能量（dB），表示接收到的信号的能量

%noise variance calculation (in dB)
[npsd_db]=ambientnoise_psd(windspeed, fc);%npsd_db是噪声的能量谱密度（单位频带内的能量）转换成dB，
nv_db=npsd_db+10*log10(bw);%相当于能量谱密度乘以带宽，即该频带内的噪声的能量
% maxamp_db和nv_db的差值应该为信噪比
disp(['Strongest tap strength=' num2str(maxamp_db,'%.1f') ' dB; Noise variance=' num2str(nv_db,'%.1f') 'dB']);

%received signal, after multipath and noise effects
[rx_signal, adj_ir_vec]=uw_isi(ir_vec, sl_db, tx_source, nv_db);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Step 3: Receiver matched-filtering
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
est_ir_vec=lfm_mf(rx_signal, tx_source, length(ir_vec));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Display of BELLHOP-simulated and estimated impulse responses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%get the time axis for plotting
x_vec=(0: length(ir_vec)-1)/sampling_rate*1000;
x_vec2=(0: length(est_ir_vec)-1)/sampling_rate*1000;

figure(1), 
plot(x_vec, abs(adj_ir_vec), 'r-', x_vec2, abs(est_ir_vec), 'b-', 'Linewidth', 2); grid on; hold on;
xlabel('Arrival time (ms)')
ylabel('Abs Amp.');
legend('BELLHOP', 'Estimate')
title('Impulse responses: BELLHOP versus Estimate');

response=abs(est_ir_vec)';
response=response/max(response);

Ts=1/sampling_rate;
t=0:Ts:T_lfm-Ts;
t2=0:Ts:(length(rx_signal)-1)*Ts;

figure(2)
subplot(2,1,1)
plot(t,tx_source)
title('LFM线性调频信号')
xlabel('时间/s')
ylabel('幅值')
subplot(2,1,2)
plot(t2,rx_signal)

title('LFM卷积后信号')
xlabel('时间/s')
ylabel('幅值')

