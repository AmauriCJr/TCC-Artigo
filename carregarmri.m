clc, clear all, format long

Y = dicomread('MRIPe.dcm');
figure
imshow(Y,[])

F = fft2(Y);
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

