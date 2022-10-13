function [NPlt, XLabels, MinVals, MaxVals] = PlotLayer(Layer, ZTop)
NPlt = 5;
XLabels = {'C_p (km/s)', 'Rho (g/cm^3)', 'C_s (km/s)', 'Alpha_p (dB/lambda)', 'Alpha_s (dB/lambda)'};

LineWidth = 2;

if Layer.IsHalfSpace
   Z = [Layer.Z Layer.Z+50];
   Cp = [Layer.Cp(1) Layer.Cp(1)];
   Rho = [Layer.Rho(1) Layer.Rho(1)];
   Cs = [Layer.Cs(1) Layer.Cs(1)];
   Ap = [Layer.Ap(1) Layer.Ap(1)];
   As = [Layer.As(1) Layer.As(1)];
else
   Z = Layer.Z;
   Cp = Layer.Cp;
   Rho = Layer.Rho;
   Cs = Layer.Cs;
   Ap = Layer.Ap;
   As = Layer.As;
end
   
subplot(1,NPlt, 1);
hold on;
plot(Cp/1000, Z+ZTop, 'LineWidth', LineWidth);
box on;

subplot(1,NPlt, 2);
hold on;
plot(Rho/1000, Z+ZTop, 'LineWidth', LineWidth);
box on;

subplot(1,NPlt, 3);
hold on;
plot(Cs/1000, Z+ZTop, 'LineWidth', LineWidth);
box on;

subplot(1,NPlt, 4);
hold on;
plot(Ap, Z+ZTop, 'LineWidth', LineWidth);
box on;

subplot(1,NPlt, 5);
hold on;
plot(As, Z+ZTop, 'LineWidth', LineWidth);
box on;

MinVals = [min(Cp/1000) min(Rho/1000) min(Cs/1000) min(Ap) min(As)];
MaxVals = [max(Cp/1000) max(Rho/1000) max(Cs/1000) max(Ap) max(As)];
