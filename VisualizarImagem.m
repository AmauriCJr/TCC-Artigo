clc, clear all, format long


fid = fopen('C:\Users\amaur\OneDrive\Documentos\Vivado\Resultado2.txt');
data = textscan(fid,'%f');
data = data{:};
fid = fclose(fid);



for j = 0:127
    x(1:128,j+1) = data((1+(128*j)):(128*(j+1)),1);
    
end


figure
y = imshow(uint8(x));
