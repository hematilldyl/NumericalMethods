function result = eulermethod(dydx,x0,y0,h,n)

for i =1:n
   y(i) = y0 + h*(dydx(x0,y0)); 
   x0 = x0+h;
   y0 = y(i);
end

result = double(y(length(y)));