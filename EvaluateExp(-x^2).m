function q3_ans = question3(num)
%Approximates the integral from 0 to 1 of exp(-x^2)

syms x
q3_ans = 0;
for n = 0:num-1;
   q3_ans= double(q3_ans+ int(((-1)^n)*(x^(2*n)/factorial(n)),x,0,1));
end
end