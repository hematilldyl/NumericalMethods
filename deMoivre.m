function roots = deMoivre(z,n)

r = sqrt(real(z)^2 + imag(z)^2);
theta = atan(imag(z)/real(z));
c = r^(1/n);
it = n-1;
for k = 0:it
    A(k+1,1) = c*(cos(theta+(2*pi()*k)/n)+(1i*sin(theta+(2*pi()*k)/n)));
end

roots = A;
end
