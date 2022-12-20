clear
clc

[H, Ht] = make_H_Ht_M();

rate = 4 / 5;
N = 1200; %% code length
origin_bits = rand(1, N) > 0.5;
origin_bits = int8(origin_bits);
ldpccodes = encode_M(origin_bits, Ht);
N = length(ldpccodes);
bpsk = ones(1, N) - 2.0 * double(ldpccodes);   

%%
%generate the puncture set
puncnum = 1200 - 2400 * (1 - rate);
randomPuncSet = randi(2400, 1, int32(puncnum * 1.25));
randomPuncSet = unique(randomPuncSet);
randomPuncSet = randomPuncSet(1 : puncnum);
puncpos = ones(1, 2400);
puncpos(randomPuncSet) = 0;


%%
%impulse noise generator



%%
noise = randn(1, N);
% noise_power = 0.01;
% noise = sqrt(noise_power) * randn(1, N);
SNR = 3;
sigma = 10^(-SNR/20);
% disp(noise(1 : 30));

% bpsk = -2 .* double(ldpccodes)  + 1;
% ldpccodes = origin_bits;
    % ISVG: H(z) = 1 + 0.599971z^(-20)
    ISVG_delay = 20;
    ISVG_amp = 0.599971;
    ISVG_multipath = [zeros(1, ISVG_delay) bpsk(1 : (N - ISVG_delay))];

    % NSVG: H(z) = 1 + 0.263112z^(-7) + 0.151214z^(-39) + 0.391599z^(-67),
    NSVG_delay1 = 7;  NSVG_delay2 = 39;  NSVG_delay3 = 67;
    NSVG_amp1 = 0.263112;  NSVG_amp2 = 0.151214;  NSVG_amp3 = 0.391599;
    NSVG_multipath1 = [zeros(NSVG_delay1,1)' bpsk(1 : (N - NSVG_delay1))];
    NSVG_multipath2 = [zeros(NSVG_delay2,1)' bpsk(1 : (N - NSVG_delay2))];
    NSVG_multipath3 = [zeros(NSVG_delay3,1)' bpsk(1 : (N - NSVG_delay3))];

    %bellhop
    Bellhop_delay1 = 0;  Bellhop_delay2 = 5;  Bellhop_delay3 = 10;
    Bellhop_delay4 = 16;  Bellhop_delay5 = 29;  Bellhop_delay6 = 44;
    Bellhop_delay7 = 57;  Bellhop_delay8 = 78;  Bellhop_delay9 = 101;
    Bellhop_amp1 = 0.3559; Bellhop_amp2 = 0.3582; Bellhop_amp3 = 0.3446;
    Bellhop_amp4 = 0.3247; Bellhop_amp5 = 0.3186; Bellhop_amp6 = 0.2937;
    Bellhop_amp7 = 0.2792; Bellhop_amp8 = 0.2617; Bellhop_amp9 = 0.2334;
    Bellhop_multipath1 = [zeros(Bellhop_delay1,1)' bpsk(1 : (N - Bellhop_delay1))];
    Bellhop_multipath2 = [zeros(Bellhop_delay2,1)' bpsk(1 : (N - Bellhop_delay2))];
    Bellhop_multipath3 = [zeros(Bellhop_delay3,1)' bpsk(1 : (N - Bellhop_delay3))];
    Bellhop_multipath4 = [zeros(Bellhop_delay4,1)' bpsk(1 : (N - Bellhop_delay4))];
    Bellhop_multipath5 = [zeros(Bellhop_delay5,1)' bpsk(1 : (N - Bellhop_delay5))];
    Bellhop_multipath6 = [zeros(Bellhop_delay6,1)' bpsk(1 : (N - Bellhop_delay6))];
    Bellhop_multipath7 = [zeros(Bellhop_delay7,1)' bpsk(1 : (N - Bellhop_delay7))];
    Bellhop_multipath8 = [zeros(Bellhop_delay8,1)' bpsk(1 : (N - Bellhop_delay8))];
    Bellhop_multipath9 = [zeros(Bellhop_delay9,1)' bpsk(1 : (N - Bellhop_delay9))];
% y = bpsk + noise;
len_code = length(ldpccodes);
% y = bpsk + ISVG_amp * ISVG_multipath + sigma * noise;
%-----------------------------------------------------------------
bpsk_awgn_err_rate = [];
bpsk_awgn_coded_err_rate = [];
%%
iter_cnt1 = [];
iter_cnt2 = [];
%for SNR = 0 : 0.1 : 4
    SNR = 4;
    origin_err_cnt = 0;
    decode_err_cnt1 = 0;
    decode_err_cnt2 = 0;
    sigma = 10 ^ (-SNR / 20);
    cnt1 = 0;
    cnt2 = 0;
    %for i = 1 : 100
        y = bpsk + sigma * noise;
        %y = bpsk + ISVG_amp * ISVG_multipath + sigma * noise;
        %y = bpsk + NSVG_amp1 * NSVG_multipath1 + NSVG_amp2 * NSVG_multipath2 + NSVG_amp3 * NSVG_multipath3 + sigma * noise;
        % y = bpsk + Bellhop_amp1 * Bellhop_multipath1 + Bellhop_amp2 * Bellhop_multipath2 + ...
        % Bellhop_amp3 * Bellhop_multipath3 + Bellhop_amp4 * Bellhop_multipath4 + Bellhop_amp5 * Bellhop_multipath5 + ...
        % Bellhop_amp6 * Bellhop_multipath6 + Bellhop_amp7 * Bellhop_multipath7 + Bellhop_amp8 * Bellhop_multipath8 + ...
        % Bellhop_amp9 * Bellhop_multipath9 + sigma * noise;
        soft_info = 2 / sigma ^ 2 * y;
        %soft_info = 2 * y;
        soft_dec = soft_info < 0;
        soft_dec = uint8(soft_dec(1 : 1200));
        origin_err_cnt = origin_err_cnt + sum(soft_dec ~= origin_bits);
        soft_info = soft_info .* puncpos;
        [code_dec1, iter_num1, mean_chan_info1] = decode_M(soft_info, H);
        [code_dec2, iter_num2, mean_chan_info2] = layerdecode_M(soft_info, H);
        decode_err_cnt1 = decode_err_cnt1 + sum(code_dec1 ~= origin_bits);
        decode_err_cnt2 = decode_err_cnt2 + sum(code_dec2 ~= origin_bits);
        cnt1 = cnt1 + iter_num1;
        cnt2 = cnt2 + iter_num2;
    %end
    %iter_cnt = [iter_cnt, cnt / 100];
%     bpsk_awgn_err_rate = [bpsk_awgn_err_rate, origin_err_cnt / 1200];
%     bpsk_awgn_coded_err_rate = [bpsk_awgn_coded_err_rate, decode_err_cnt / 1200];
%end
% y = bpsk + sigma * noise;
% soft_info = 2 / sigma ^ 2 * y;
% soft_dec = soft_info < 0;
% soft_dec = uint8(soft_dec(1 : 1200));


% fprintf('before decoding, the error bit cnt = %d\n', sum(soft_dec ~= origin_bits));
% code_dec = decode_M(soft_info, H);
% fprintf('after decoding, the error bit cnt = %d\n', sum(code_dec ~= origin_bits));