function coeff = question4(m,n)
%Polynomial Regression Q4

fitData = m;
terms= n;
totalTerms=terms+1; % max term
[rl,cl]=size(fitData);

% initialize Z
for zRows=1:totalTerms
    for zColumns=1:totalTerms
    z(zRows,zColumns)=0;
        for i=1:rl
        z(zRows,zColumns)=z(zRows,zColumns)+fitData(i,1)^((zColumns-1)+(zRows-1));
        end
    end
end

% intitialize Y
for rows=1:totalTerms
   y(rows)=0;
   for i=1:rl
     y(rows)=y(rows)+fitData(i,2)*fitData(i,1)^(rows-1);
   end
end
coeff =inv(z)*y';
end
