clc, clear all, format long
%

x = [1 2 5 10
     1 4 80 1
     1000 2 7 3
     2 1 1 1]
 
 N = length(x);
 N2 = N/2;
 T = 1

for i = 1+T:N-T
    y(:,i-T) = x(:,i)
end
for i = 1+T:N-T
    z(i-T,:) = y(i,:)
end
