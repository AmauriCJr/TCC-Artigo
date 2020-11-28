clc, clear all, format long


img = imread('C:\Users\amaur\OneDrive\Área de Trabalho\Eletrônica\TCC1\Yoda.jpg');
X = img;

img = imresize((img),[128 128]);

t = img(:,:,1)
figure
y = imshow(t);

img;

FFT = fft2(t);



% F = abs(fftshift(FFT));
% 
% % 
% % X = ifft2(F);
% 
% % figure
% % y = imshow(X);

Sat = 65535;

Re = real(FFT);
% Re(Re>Sat) = Sat;
% Re(Re<-Sat) = -Sat;

Im= imag(FFT);
% Im(Im>Sat) = Sat;
% Im(Im<-Sat) = -Sat;



X = Re + Im*i;

% X = uint8(X);

X = ifft2(X);

% T = ifft2 (FFT)
% K = ifft2 (X);

% img2 = uint8(K);

imag2 = imag(X);

real2 = real(X);

figure
Y = imshow(uint8(X));


fid = fopen('C:\Users\amaur\OneDrive\Documentos\Vivado\PeRe.coe', 'wt');
fprintf(fid,'memory_initialization_radix=10;\n');
fprintf(fid,'memory_initialization_vector=');
fprintf(fid, '%0.f,', Re);
fclose(fid)


fid = fopen('C:\Users\amaur\OneDrive\Documentos\Vivado\PeIm.coe', 'wt');
fprintf(fid,'memory_initialization_radix=10;\n');
fprintf(fid,'memory_initialization_vector=');
fprintf(fid, '%.0f,', Im);
fclose(fid)


