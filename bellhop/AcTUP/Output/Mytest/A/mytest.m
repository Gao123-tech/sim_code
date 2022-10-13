clear
clc
filename = 'Test_20000.arr';
Minimum_range=100  %������ˮ������ˮƽ�����Ͻ��շ�Χ��Сֵ��m��----R 
Maximum_range=2000 %������ˮ������ˮƽ�����Ͻ��շ�Χ���ֵ��m��---RB 

[ amp1, delay, SrcAngle, RcvrAngle, NumTopBnc, NumBotBnc, narrmat, Pos ]... 
 = read_arrivals_asc(  filename );
%%��λ�弤��Ӧ
[m,n]=size(amp1);
amp = abs(amp1); %ȡģ  
x = delay(m,:); %��ȡ���һ�����ջ���ʱ�Ӻͷ�ֵ
y = amp(m,:);
figure(1)
stem(x,y)
grid on
xlabel('���ʱ��/s')
ylabel('����')
title('��λ�弤��Ӧ')

%%��һ���弤��Ӧ
Amp_Delay = [x;y];
Amp_Delay(:,all(Amp_Delay==0,1))=[]; %ȥ��0ֵ
Amp_Delay=sortrows(Amp_Delay',1);  %����ʱ�Ӵ�С��������
normDelay = Amp_Delay(:,1)-Amp_Delay(1,1);%��һ��ʱ��
normAmp = Amp_Delay(:,2)/Amp_Delay(1,2);%��һ������
figure(2)
stem(normDelay,normAmp,'^')
grid on
xlabel('���ʱ��/s')
ylabel('��һ������')
title('��һ���弤��Ӧ')
figure(3)
mum=1:m;
ReRange = Minimum_range+(Maximum_range-Minimum_range)/m*mum;
for i=1:min(narrmat)
plot(delay(:,i),ReRange,'o')
hold on
end
hold off
grid on
colorbar
xlabel('ʱ��(sec)')
ylabel('Range(m)')
title(filename)

