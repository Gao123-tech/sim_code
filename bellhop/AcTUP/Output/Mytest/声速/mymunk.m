z = 0:10:2000;
c0 = 1500;
e = 0.57e-2;
B=1000;
z0 = 1000;
a = 2*(z-z0)/B;
c = c0*(1+e*(exp(-a)-(1-a)));
plot(c,z)
grid on
xlabel('speed (m/s)')
ylabel('deep');
title('Munk');
init_ssp = [z',c'];
multi_ssp = repmat(c',[1,length(z)]);
num = 8;
st = length(z);
ssp_range_vec=linspace(0,num,st)';

