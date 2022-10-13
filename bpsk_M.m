clc
clear

%% 基本参数
fs = 200e6; %% 采样频率
Ts = 1 / fs; %% 采样时间
fc = 25e6; %% 载波频率
Rb = 1.25e6;
Tb = 1 / Rb;
%% send_data = "I don't know who you are, je ne sais plus comment tu t'appelles?";

%% 调用mex程序完成encode
[H, Ht] = make_H_Ht_M();
iter = 1;
err_cnt_mat = [];
err_cnt_mat_ldpc = [];
% for SNR = -5 : 0.1 : 5
    total_err_cnt = 0;
    total_err_cnt_ldpc = 0;
%     for iter = 1 : 100
        N = 1200; %% code length
        origin_bits = rand(1, N) > 0.5;
        origin_bits = int8(origin_bits);
        ldpccodes = encode_M(origin_bits, Ht);
        % ldpccodes = origin_bits;
        len_code = length(ldpccodes);

        %% ----------------------------------------------------


        %% 调制
        bpsk = [];
        t = 0:Ts:(Tb - Ts);

        for i = 1:len_code

            if ldpccodes(i) == 0
                bpsk = [bpsk, cos(2 * pi * fc * t)];
            else
                bpsk = [bpsk, cos(2 * pi * fc * t + pi)];
            end

        end

        % figure(1)
        % plot(bpsk)
        %% 向调制信号加入高斯白噪声
        SNR = -5;
        bpsk_power = mean(bpsk .^ 2);
        fprintf('bpsk power = %f', bpsk_power);
        receive_sig = awgn(bpsk, SNR, 'measured');
        %receive_sig = bpsk + wgn(size(bpsk), 0);
        noise = receive_sig - bpsk;
        noise_power = mean(noise .^ 2);
        fprintf('\nnoise power = %f', noise_power);
        fprintf('\nsnr = %f', 10 * log10(bpsk_power / noise_power));
        % figure(2)
        % plot(receive_sig)

        %% 运用bellhop水声信道模型

        %% 解调
        %乘法器
        receive_T1 = reshape(receive_sig, 160, []);
        receive_T2 = cos(2 * pi * fc * t)' .* receive_T1;
        receive_T3 = reshape(receive_T2, 1, []);
        % figure(3)
        % plot(receive_T3)

        wp = 20e6 / (fs / 2);
        ws = 40e6 / (fs / 2);
        rp = 3;
        rs = 20;
        [n, wn] = buttord(wp, ws, rp, rs);
        [b, a] = butter(n, wn);
        demodulation = filter(b, a, receive_T3);
        % figure(4)
        % plot(demodulation)
        soft_dec = 2 .* demodulation(1, 160 .* (1 : 2400)) .* 2; %% input parameter of function decoder_M
        error_bit_cnt = 0;
        for i = 1 : 1200
            if ((soft_dec(i) < 0) ~= origin_bits(i))
                error_bit_cnt = error_bit_cnt + 1;
            end
        end
        total_err_cnt = total_err_cnt + error_bit_cnt;
        fprintf('\nBefore fec, the error bit cnt = %d\n', error_bit_cnt);
        % decode_M(soft_dec);
        code_dec = decode_M(soft_dec, H);
        error_bit_cnt = sum(code_dec ~= origin_bits);
        fprintf('after decoding, the error bit cnt = %d\n', error_bit_cnt);
        total_err_cnt_ldpc = total_err_cnt_ldpc + error_bit_cnt;
        %iter = iter + 1;
    %     pause(0.5);
%     end 
    err_cnt_mat = [err_cnt_mat, total_err_cnt];
    err_cnt_mat_ldpc = [err_cnt_mat_ldpc, total_err_cnt_ldpc];
% end