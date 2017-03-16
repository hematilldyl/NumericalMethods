function yresult = lab3(x)
%{
    Author: Dylan Hematillake
    Date: March 8th, 2017
    Program Description: Uses the lagrange interpolation
                         method to interpolate data
%}
%n is 5 for this
%otherwise n = length(xVal)-1

%measure values
xVal = [1 2 3 4 5 6];
yVal = [0 6 2 1 5 10];

sum = 0;

%MATLAB indexing begins a 1 instead of 0 
for i = 1:6
    psum = 1;
    for j = 1:6
        if(j ~= i)
            psum = (x-xVal(j))/(xVal(i)-xVal(j)) *psum;
        end
    end
    sum = (yVal(i)*psum)+sum;
end

yresult = sum;
end