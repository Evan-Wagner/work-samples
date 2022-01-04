
%% A4 pure tone, square, sawtooth
clear; clc;

FS = 44100;
secs = 1.5;
T = 1/440;

t = linspace(0,secs,FS*secs);
x = sin(2*pi*t/T);
sq = square(2*pi*t/T);
st = sawtooth(2*pi*t/T);

plot(t,x,'k-')
%plot(t,sq,'k-')
%plot(t,st,'k-')
axis([0 4*T -1.5 1.5])

sound1 = audioplayer(1/20*x,FS);
sound2 = audioplayer(1/20*sq,FS);
sound3 = audioplayer(1/20*st,FS);

playblocking(sound1)
playblocking(sound2)
playblocking(sound3)

%audiowrite("audio/pure_tone_A4.wav",1/20*x,FS)
%audiowrite("audio/square_A4.wav",1/20*sq,FS)
%audiowrite("audio/sawtooth_A4.wav",1/20*st,FS)


%% pure tone cluster and sum
clear; clc;

FS = 44100;
secs = 1.5;

t = linspace(0,secs,FS*secs);

x1 = 2*sin(2*pi*t*90); x2 = -5/3*cos(2*pi*t*120); x3 = 4/7*cos(2*pi*t*165);
x = x1 + x2 + x3;

T = 1/15;

hold on
    %plot(t,x1,'k-',color='green')
    %plot(t,x2,'k-',color='magenta')
    %plot(t,x3,'k-',color='cyan')

    plot(t,x,'k-')

    %axis([0 T -5 5])
    axis([0 1 -5 5])
hold off

soundcluster = audioplayer(1/2*[x1 zeros(1,FS*.4) x2 zeros(1,FS*.4) x3],FS);
soundsum = audioplayer(1/2*x,FS);

playblocking(soundcluster)
playblocking(soundsum)

%audiowrite("audio/pure_tone_cluster.wav",1/2*[x1 zeros(1,FS*.4) x2 zeros(1,FS*.4) x3],FS);
%audiowrite("audio/pure_tone_sum.wav",1/2*x,FS);

%% linearity of real fourier

T = 1;

t1 = 0;
t2 = T/8;
t3 = T/4;
t4 = T/2;
t5 = 3*T/4;


M = [1 cos(2*pi*t1/T) cos(4*pi*t1/T) sin(2*pi*t1/T) sin(4*pi*t1/T);
     1 cos(2*pi*t2/T) cos(4*pi*t2/T) sin(2*pi*t2/T) sin(4*pi*t2/T);
     1 cos(2*pi*t3/T) cos(4*pi*t3/T) sin(2*pi*t3/T) sin(4*pi*t3/T);
     1 cos(2*pi*t4/T) cos(4*pi*t4/T) sin(2*pi*t4/T) sin(4*pi*t4/T);
     1 cos(2*pi*t5/T) cos(4*pi*t5/T) sin(2*pi*t5/T) sin(4*pi*t5/T);]

rref(M)

%% fourier approx of square wave
clear; clc; clf;

FS = 44100;
secs = 1.5;
T = 1/440;
t = linspace(0,secs,FS*secs)

N = 1000;

y = zeros(1,length(t));
for n=1:2:N
    y = y + (4/(pi*n))*sin(2*pi*n*t/T);
end

plot(t, y, 'k-')
axis([0 4*T -1.5 1.5])

square_approx = audioplayer(1/20*y,FS);
playblocking(square_approx)

%audiowrite('audio/square_approx_1000.wav',1/20*y,FS)

%% LPF and HPF (digital)
clear; clc;

N = 100; K=5;

lambda_LPF = cat(2,ones(1,K),zeros(1,N-2*K),ones(1,K));
lambda_HPF = cat(2,zeros(1,K),ones(1,N-2*K),zeros(1,K));

T = 1/440;
FS = N/T;
t = linspace(0,T,FS*T);

%x = square(2*pi*t/T).';
%x = (sawtooth(2*pi*t/T)).';
%x = (cos(2*pi*t/T) + cos(4*pi*t/T)).';
x = audioread('audio/trumpetA4.wav',[1, FS*T]); x = x(:,1) + x(:,2);

x_LPF = filter(x,lambda_LPF);
x_HPF = filter(x,lambda_HPF);

DFTx = 1/sqrt(N)*dft_impl(x);
DFTx_LPF = diag(lambda_LPF)*DFTx;
DFTx_HPF = diag(lambda_HPF)*DFTx;

secs = 1.5;
fullt = linspace(0,secs,FS*secs);
fullx = repmat(x,secs/T,1);
fullx_LPF = repmat(x_LPF,secs/T,1);
fullx_HPF = repmat(x_HPF,secs/T,1);

hold on
    %plot(fullt,real(fullx)+imag(fullx),'-k')
    %plot(fullt,real(fullx_LPF)+imag(fullx_LPF),'-k',color='b')
    %plot(fullt,real(fullx_HPF)+imag(fullx_HPF),'-k',color='r')

    stem(fullt,real(fullx)+imag(fullx))
    stem(fullt,real(fullx_LPF)+imag(fullx_LPF),color='b')
    stem(fullt,real(fullx_HPF)+imag(fullx_HPF),color='r')

    %stem(t,real(DFTx)+imag(DFTx),color='magenta')
    %stem(t,real(DFTx_LPF)+imag(DFTx_LPF),color='b')
    %stem(t,real(DFTx_HPF)+imag(DFTx_HPF),color='r')

    axis([0 T -2.5 2.5])
hold off

sound1 = audioplayer(1/10*(real(fullx)+imag(fullx)),FS);
sound2 = audioplayer(1/10*(real(fullx_LPF)+imag(fullx_LPF)),FS);
sound3 = audioplayer(1/10*(real(fullx_HPF)+imag(fullx_HPF)),FS);

playblocking(sound1)
playblocking(sound2)
playblocking(sound3)

%audiowrite('audio/trumpetA4_sample.wav',1/10*(real(fullx)+imag(fullx)),FS);
%audiowrite('audio/trumpetA4_LPF.wav',1/10*(real(fullx_LPF)+imag(fullx_LPF)),FS);
%audiowrite('audio/trumpetA4_HPF.wav',1/10*(real(fullx_HPF)+imag(fullx_HPF)),FS);

%% DFT matrix LPF and HPF
clear; clc; clf;

N = 1000; K = .1*N;
FN = dft_matrix(N);

lambda_LPF = cat(2,ones(1,N-K),zeros(1,K));
D_LPF = diag(lambda_LPF);
LPF = inv(FN)*D_LPF*FN;

lambda_HPF = cat(2,zeros(1,K),ones(1,N-K));
D_HPF = diag(lambda_HPF);
HPF = inv(FN)*D_HPF*FN;

FS = N;
T = 1/44;
secs = 1.5;
t = linspace(0,secs,FS*secs);

x = square(2*pi*t/T).';
%x = sawtooth(2*pi*t/T).';

x_LPF = LPF*x;
x_HPF = HPF*x;

hold on
    %plot(t,real(x)+imag(x),'-k')
    %plot(t,real(x_LPF)+imag(x_LPF),'-k',color='b')
    %plot(t,real(x_HPF)+imag(x_HPF),'-k',color='r')

    stem(t,real(x)+imag(x))
    stem(t,real(x_LPF)+imag(x_LPF),color='b')
    stem(t,real(x_HPF)+imag(x_HPF),color='r')

    axis([0 4*T -1.5 1.5])
hold off

%% LPF and HPF (analog)
clear; clc;

FS = 44100;
secs = 1.5;
T = 1/440;
t = linspace(0,secs,FS*secs);

x1 = exp(2*pi*1i*1*t/T); x2 = exp(2*pi*1i*2*t/T); x3 = exp(2*pi*1i*3*t/T);
x4 = exp(2*pi*1i*4*t/T); x5 = exp(2*pi*1i*5*t/T); x6 = exp(2*pi*1i*6*t/T);

hold on
    %plot(t,real(x1)+imag(x1),color='b')
    %plot(t,real(x2)+imag(x2),color='b')
    %plot(t,real(x3)+imag(x3),color='b')
    %plot(t,real(x4)+imag(x4),color='r')
    %plot(t,real(x5)+imag(x5),color='r')
    %plot(t,real(x6)+imag(x6),color='r')
    %axis([0 1*T -2 2])
hold off

x = 1/6*(x1 + x2 + x3 + x4 + x5 + x6).';
x_LPF = 1/6*(x1 + x2 + x3).';
x_HPF = 1/6*(x4 + x5 + x6).';

hold on
    plot(t,real(x)+imag(x))
    plot(t,real(x_LPF)+imag(x_LPF),col='b')
    plot(t,real(x_HPF)+imag(x_HPF),col='r')
    axis([0 1*T -2 2])
hold off

sound1 = audioplayer(1/10*(real(x)+imag(x)),FS);
sound2 = audioplayer(1/10*(real(x_LPF)+imag(x_LPF)),FS);
sound3 = audioplayer(1/10*(real(x_HPF)+imag(x_HPF)),FS);

playblocking(sound1)
playblocking(sound2)
playblocking(sound3)

%audiowrite('audio/sixth_order.wav',1/10*(real(x)+imag(x)),FS);
%audiowrite('audio/sixth_order_LPF.wav',1/10*(real(x_LPF)+imag(x_LPF)),FS);
%audiowrite('audio/sixth_order_HPF.wav',1/10*(real(x_HPF)+imag(x_HPF)),FS);

%% functions

function FN = dft_matrix(N)
    FN = zeros(N);
    for n = 0:(N-1)
        for k = 0:(N-1)
            FN(n+1,k+1) = 1/sqrt(N)*exp(-2*pi*1i*n*k/N);
        end
    end
end

function y = dft_impl(x)
    N = size(x, 1);
    y = zeros(size(x));
    for n = 1:N
        D = exp(2*pi*1i*(n-1)*(0:(N-1))/N);
        y(n) = dot(D, x);
    end
end

function y = idft_impl(x)
    N = size(x, 1);
    y = zeros(size(x));
    for n = 1:N
        D = exp(-2*pi*1i*(n-1)*(0:(N-1))/N);
        y(n) = dot(D, x);
    end
end

function xnew = filter(x,lambda)
    y = dft_impl(x);
    ynew = zeros(length(y),1);
    for n = 1:length(y)
        ynew(n) = lambda(n)*y(n);
    end
    xnew = 1/length(x)*idft_impl(ynew);
end
