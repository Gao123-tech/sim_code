fs = 500; %采样率
f1 = 5; %信号频率
f2 = 10; %信号频率
T = 1; %时宽1s
n = round(T * fs); %采样点个数
t = linspace(0, T, n); %时域横坐标
delay = 0.3;
x = 3 + cos(2 * pi * f1 * t) + 0.5 .* cos(2 * pi * f1 * (t + delay)); %形成三频信号,注意第二个频率信号幅度为2，直流幅度为3
figure(1);
plot(t, x); %画时域图
xlabel("t/s")
grid on

X = fftshift(fft(x ./ (n))); %用fft得出离散傅里叶变换
f = linspace(-fs / 2, fs / 2 - 1, n); %频域横坐标，注意奈奎斯特采样定理，最大原信号最大频率不超过采样频率的一半
figure(2)
plot(f, abs(X)); %画双侧频谱幅度图
xlabel("f/Hz")
ylabel("幅度")
grid on
