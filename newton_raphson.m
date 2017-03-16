function q1_ans =question1(gv, sc)
%Newton-Raphson Q1 Exam
syms x
func(x) = exp(-x)-x;
funcP(x) = diff(func,x);
X(1) = gv;

ea = 100;
numIts = 1;

while ea>sc
    numIts = numIts+1;
    X(numIts) = X(numIts-1)-func(X(numIts-1))/funcP(X(numIts-1));
    ea(numIts) = abs((X(numIts)-X(numIts-1))/X(numIts))*100;
end

q1_ans = X(numIts);

end

