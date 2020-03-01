clc, clear all, format long

folder = 'C:\Users\amaur\OneDrive\Área de Trabalho\Eletrônica\TCC1';

F = dlmread(fullfile(folder, 'FFT.txt'));
    
figure                  
colormap(gray(1024))           
imagesc(abs(fftshift(F)))
axis image 

D = abs(ifft2(F));

figure                  
colormap(gray(1024))           
image(D) 
axis image 
