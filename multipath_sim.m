fs = 20000;
f1 = 5000;
N = 10000;
t = 0 : 1 / fs : (N - 1) / fs;
y1 = sin(2 * pi * f1 * t);
plot(t, y1);