clc, clear all, format long

Y = dicomread('MRIPe.dcm');
figure
imshow(Y,[])

N = length(Y)

T = 24

for i = 1+T:N-T
    y(:,i-T) = Y(:,i);
end
for i = 1+T:N-T
    z(i-T,:) = y(i,:);
end

figure
imshow(z,[])

F = fft2(z);
FFT = abs(fftshift(F));
                 
figure                  
colormap(gray(1024))          
imagesc(abs(fftshift(F)))
axis image

folder = 'C:\Users\amaur\OneDrive\Área de Trabalho\Eletrônica\TCC1';

dlmwrite(fullfile(folder, 'FFT.txt'), F)

D = ifft2(F);

figure                  
colormap(gray(1024))           
image(D) 
axis image 


