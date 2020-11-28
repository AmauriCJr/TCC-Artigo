clc, clear all, format long


fid = fopen('C:\Users\amaur\OneDrive\Documentos\Vivado\Resultado2.txt');
data = textscan(fid,'%f');
data = data{:};
fid = fclose(fid);



for j = 0:127
    x(1:128,j+1) = data((1+(128*j)):(128*(j+1)),1);
    
end
k = uint8(x);

figure
y = imshow(k);


img = imread('C:\Users\amaur\OneDrive\Área de Trabalho\Eletrônica\TCC1\Pe.png');
X = img;

img = imresize((img),[128 128]);

t = img(:,:,1);
figure
y = imshow(t);

res = k - t;

figure
y = imshow(res);

Original = sum(t(:)) / 16384
Recriada = sum(k(:)) / 16384
S = sum(res(:)) / 16384
