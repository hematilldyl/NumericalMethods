%{
Gauss Seidel
    Author: Dylan Hematillake
    Date: Feb. 2nd 2017
    Program Description: 
        The program runs the Gauss_Seidel Iterative method to solve a 
        system of equations. It can take user input for the matrix and 
        the allowed error. It will run a default matrix if the user does
        not want to enter one.
%}

convergeS = input('Input stopping criteria as a decimal: '); 
numIts = 1;
MAX_ITERATIONS = 100;

userAgree = false;
%default matrix
A = [3 -2 1;1 -3 2; -1 2 4];
b = [2; 5; 6];
[r,c] =size(A);

while(~userAgree)
    in = inputdlg('Would you like to input a matrix to solve? Y/N');
    if(~strcmp(in,'Y'))
        break;
    end
dimRow = input('Number of rows in coefficient matrix: ');
dimCol = input('Number of columns in coefficient matrix: ');
A = zeros(dimRow,dimCol);
b = zeros(dimRow,1);
for i = 1:dimRow
    for j = 1:dimCol
        A(i,j) = input('Input the element of the matrix: ');
        if(j == dimCol)
            b(i) = input('Input the value the equation equals (b vector): ');
        end
    end
end
disp('Your coefficient matrix is: ');
disp(A);
disp('Your b vector is: ');
disp(b);
check = inputdlg('Is this correct? Y/N');
[r,c] =size(A);
if(strcmp(check,'Y'))
     userAgree = true;
if(r ~= c || det(A) == 0)
    disp('INVALID MATRIX, MUST BE INVERTIBLE');
    userAgree = false;
end
n=length(b);
for i = 1:n
    j = 1:n;
    j(i) = [];
    C = abs(A(i,j));
    checker(i,1)= abs(A(i,i))- sum(b);
    if(checker(i) < 0)
        disp('GAUSS SEIDEL WORKS ONLY FOR DIAGONALLY DOMINANT MATRICES');
        disp('ENTER AGAIN.');
        userAgree=false;
        break;
    end
end
end
end

x = zeros(1,c);
disp(x);
C = zeros(r,c);
for i = 1:r 
    for j =1:c
        C(i,j) = -1*(A(i,j)/(A(i,i)));
        if(i == j)
            C(i,j) = 0;
        end
    end
end
%x = Cx + d
while(MAX_ITERATIONS > convergeS)
    numIts = numIts + 1;
    for j = 1:r
        x(numIts, j) = b(j)/A(j,j);
        for k = 1:c
           %jacobi has no if conditions, jump to else statement
          if k <= j
              x(numIts, j) = x(numIts, j) + C(j,k)*x(numIts,k);
          else
               x(numIts,j) = x(numIts, j)+ C(j,k) * x(numIts-1, k);
          end
        end
    end
    MAX_ITERATIONS(numIts-1) = abs((x(numIts,1)-x(numIts-1,1))/x(numIts,1))*100;
end
A = ['      x1   ' '     x2   ' '     x3   '];
if(r == 3)
    disp(A)
    disp('   ----------------------------')
end
disp(x)

