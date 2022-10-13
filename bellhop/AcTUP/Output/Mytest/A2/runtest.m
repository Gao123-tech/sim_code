bellhopM( 'test' )
load test.mat
%%��λ�弤��Ӧ
[p,m,n]=size(DelArr);
delay=reshape(DelArr,m,n);
amp1=reshape(AArr,m,n);
amp = abs(amp1); %ȡģ  
x = delay(m,:); %��ȡ��50�����ջ���ʱ�Ӻͷ�ֵ
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



