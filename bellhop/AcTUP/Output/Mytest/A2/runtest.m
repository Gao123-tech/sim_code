bellhopM( 'test' )
load test.mat
%%单位冲激响应
[p,m,n]=size(DelArr);
delay=reshape(DelArr,m,n);
amp1=reshape(AArr,m,n);
amp = abs(amp1); %取模  
x = delay(m,:); %获取第50个接收机的时延和幅值
y = amp(m,:);
figure(1)
stem(x,y)
grid on
xlabel('相对时延/s')
ylabel('幅度')
title('单位冲激响应')

%%归一化冲激响应
Amp_Delay = [x;y];
Amp_Delay(:,all(Amp_Delay==0,1))=[]; %去掉0值
Amp_Delay=sortrows(Amp_Delay',1);  %按照时延从小到大排序
normDelay = Amp_Delay(:,1)-Amp_Delay(1,1);%归一化时延
normAmp = Amp_Delay(:,2)/Amp_Delay(1,2);%归一化幅度
figure(2)
stem(normDelay,normAmp,'^')
grid on
xlabel('相对时延/s')
ylabel('归一化幅度')
title('归一化冲激响应')



