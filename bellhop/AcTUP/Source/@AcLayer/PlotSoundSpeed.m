function [NPlt, XLabels] = PlotSoundSpeed(Layer, ZTop)
NPlt = 5;
XLabels = {'Cp (m/s)', 'Rho (kg/m^3)', 'Cs (m/s)', 'Alpha_p (dB/lambda)', 'Alpha_s (dB/lambda)'};

subplot(1,NPlt, 1);
hold on;
plot(Layer.Cp, Layer.Z+ZTop);

subplot(1,NPlt, 2);
hold on;
plot(Layer.Rho, Layer.Z+ZTop);

subplot(1,NPlt, 3);
hold on;
plot(Layer.Cs, Layer.Z+ZTop);

subplot(1,NPlt, 4);
hold on;
plot(Layer.Ap, Layer.Z+ZTop);

subplot(1,NPlt, 5);
hold on;
plot(Layer.As, Layer.Z+ZTop);