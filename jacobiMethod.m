function xi=jacobiMethod(a, b, x, es)

[row,column]=size(a); 

for n=1:row
    for i=1:column
        c(n,i)=-1*a(n,i)/a(n,n);

if n==i
    c(n,i)=0;
end
    end

end

ea=100;
iterations=1;

while ea>es
iterations=iterations+1;
    for j=1:row
        x(j,iterations)=b(j)/a(j,j);
        for k=1:column 
            x(j,iterations)=x(j,iterations)+c(j,k)*x(k, iterations-1);
        end
    end
ea(iterations-1)=abs((x(1,iterations)-x(1,iterations-1))/x(1,iterations))*100;
end

xi=x(:,iterations);
