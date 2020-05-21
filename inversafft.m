function X = inversafft(x)
    N = length(x);
    if N == 1
        X = x;
    else
        N2 = N/2;
        X_par = inversafft(x(1:2:N-1));
        X_impar = inversafft(x(2:2:N));
        W = exp(-2 * pi * 1i / N) .^ (0:N2-1)
        X_impar = W .* X_impar;
        X = [ (X_par + X_impar)/2 , (X_par - X_impar)/2 ];
    end
end
