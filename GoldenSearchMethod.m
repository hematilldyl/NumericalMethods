function max_ans = question2(x1,x2,sc)
%Golden search method for max value of a function in a set range

numIts = 1;

%initialize function
syms x
f(x) = 2*sin(x)-(x^2/10);

ea = 100;

%phi
gr = 0.61803;
%iterative process
while(ea>sc)
    numIts = numIts+1;
    d = gr*(x2-x1);
    X1 = x1 +d;
    X2 = x2 -d;
    if(f(X1)>f(X2))
        x1= X2;
        xopt = X1;
    elseif(f(X1)<f(X2))
        x2=X1;
        xopt = X2;
    end
    ea(numIts)=(1-gr)*abs((x2-x1)/xopt)*100;
end
max_ans=xopt;
end
