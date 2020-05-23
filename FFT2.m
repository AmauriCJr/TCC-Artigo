clc, clear all, format long

folder = 'C:\Users\amaur\OneDrive\Área de Trabalho\Eletrônica\TCC1';

x = dlmread(fullfile(folder, 'FFT.txt'));
    
figure                  
colormap(gray(1024))           
imagesc(abs(fftshift(x)))
axis image 

figure                  
colormap(gray(1024))           
imagesc(abs(fftshift(x)))
axis image 

N = length(x)

for i = 1:N
    j(:,i) = inversafft(x(:,i)); 
end
for i = 1:N
    z(i,:) = inversafft(j(i,:)); 
end

z = flipud(z);

figure                  
colormap(gray(1024))           
image(abs(z)) 
axis image 
 

