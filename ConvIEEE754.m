clc, clear all, format long


x = -987.8466;


zero = '0';

um = '1';

if (x > 0)
res = dec2bin(typecast(single(x), 'int32'));
s = strcat(zero,res)
elseif (x < 0)
x = abs(x);
res = dec2bin(typecast(single(x), 'int32'));
s = strcat(um,res)
elseif (x == 0)
s = '00000000000000000000000000000000'
end
j = 0;

for i = 1:23
j = j+1;    
bases(j) = 1/(2^i);

end

exp = s(2:9);

man = s(10:32);

total = 0;




for i = 1:23
Value = sscanf(man(i), '%d'); 
decimal = bases(i)*Value;
total = decimal + total; 
end

total = total + 1;

expd = bin2dec(exp);

mand = bin2dec(man);

pot = 2^(expd-127);


sinal = s(1)
if (sinal == '1')
    mult = -1
elseif(sinal == '0')
    mult = 0
end

Resultado = mult*pot*total


