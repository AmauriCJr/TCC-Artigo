clc, clear all, format long


x = [1 1 1 10
     1 4 80 1
     1000 1 1 3
     2 1 1 1]
 
N = length(x)
% y = ifft(x)
% X = inversafft(x)
% Y = conj(X)


for i = 1:N
    y(:,i) = inversafft(x(:,i)); 
end
for i = 1:N
    z(i,:) = inversafft(y(i,:)); 
end
z
X = ifft2(x)

figure
image(abs(z))

figure
image(abs(X))
