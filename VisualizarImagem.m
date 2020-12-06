clc, clear all, format long


fid = fopen('C:\Users\amaur\OneDrive\Documentos\Vivado\Resultado.txt');
data = textscan(fid,'%f');
data = data{:};
fid = fclose(fid);



for j = 0:127
    x(1:128,j+1) = data((1+(128*j)):(128*(j+1)),1);
    
end


k = uint8(x);


figure
y = imshow(k);


img = imread('C:\Users\amaur\OneDrive\Área de Trabalho\Eletrônica\TCC1\MRIBrain.jpg');
X = img;

img = imresize((img),[128 128]);

t = img(:,:,1);
figure
y = imshow(t);


[K,ssimmap] = ssim(k,t);

figure
imshow(ssimmap,[])
title(['Coeficiente de Semelhança: ',num2str(K)])
