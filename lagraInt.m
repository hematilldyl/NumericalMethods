function yresult = lagraInt(x)
%{
    Author: Dylan Hematillake
    Date: March 8th, 2017
    Program Description: Uses the lagrange interpolation
                         method to interpolate csv data
%}


%measured values
%xVal = [1 2 3 4 5 6];
%yVal = [0 6 2 1 5 10];

%change filename depending on location
filename = 'd:\MatlabPrograms\data.csv';
delim = ',';
headerln = 1;
datapoints = importdata(filename,delim,headerLn);
[xVal,yVals=[datapoints.data(:,1),datapoints.data(:,2)]

n = length(xVal)
sum = 0;

for i = 1:n
    psum = 1;
    for j = 1:n
        if(j ~= i)
            psum = (x-xVal(j))/(xVal(i)-xVal(j)) *psum;
        end
    end
    sum = (yVal(i)*psum)+sum;
end

yresult = sum;
end
