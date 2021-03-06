!Change the dydt as needed for the desired IVP ODE
!Contains explicit and implicit methods
MODULE ExplicitMethods

CONTAINS
SUBROUTINE EulerMethod(y0,x0,Ns)
    REAL :: y0, x0,h
    INTEGER, INTENT(IN) :: Ns
    INTEGER :: i
    REAL, ALLOCATABLE, DIMENSION(:) :: Y
	h = 1.0/Ns

	ALLOCATE(Y(Ns))
    
	DO i=1,Ns
   		Y(i) = y0 + h*(dydx(x0,y0))
        x0 = x0+h
        y0 = Y(i)
    	IF (i==Ns) THEN
        	PRINT*,Y(i)
        END IF
	END DO
    
	DEALLOCATE(Y)
	   
	RETURN
    
    CONTAINS 
    	REAL FUNCTION dydx(x,y)
        	IMPLICIT NONE
        	REAL, INTENT(IN) :: x,y
        	dydx = -21.6*y+0*x
        END FUNCTION dydx
   
END SUBROUTINE EulerMethod

!yi+1=yi+phi*h
	!2nd order: phi = a1k1+a2k2, k1 = f(xi,yi),k2=(xi+pih,yi+q11k1h)
 	!a1+a2=1, a2*p1=0.5, a2*q11 = 0.5
	!Heun's a2=0.5
    !Midpoint a2=1
    !Ralston's a2=2/3

!k1 = f(xi,yi), k2 = f(xi+h/2,yi+h/2*k1), yi+1=yi+k2*h    
SUBROUTINE MidpointMethod(y0,t0,Ns)
    REAL :: y0, t0,h,k1,k2
    INTEGER, INTENT(IN) :: Ns
    INTEGER :: i
    REAL, ALLOCATABLE, DIMENSION(:) :: Y
	h = 1.0/Ns

	ALLOCATE(Y(Ns))
    
	DO i=1,Ns
    	k1=dydt(t0,y0)
        k2=dydt(t0+h/2,y0+(h/2)*k1)
   		Y(i) = y0 + h*k2
        t0 = t0+h
        y0 = Y(i)
        PRINT*,Y(i)
    	IF (i==Ns) THEN
        	PRINT*,Y(i)
        END IF
	END DO
    
	DEALLOCATE(Y)
	   
	RETURN
    
    CONTAINS 
    	REAL FUNCTION dydt(t,y)
        	IMPLICIT NONE
        	REAL, INTENT(IN) :: t,y
        	dydt = y*t*t*t-1.5*y
        END FUNCTION dydt
        
END SUBROUTINE MidpointMethod


SUBROUTINE HeunsMethod(y0,t0,Ns)
    REAL :: y0,t0,h,y1,k1,k2
    INTEGER, INTENT(IN) :: Ns
    INTEGER :: i
    REAL, ALLOCATABLE, DIMENSION(:) :: Y
	h = 1.0/Ns

	ALLOCATE(Y(Ns))
	DO i=1,Ns
    	k1=dydt(t0,y0)
        y1=y0+(k1*h)
        k2=dydt(t0+h,y1)
   		Y(i) = y0 + ((k1+k2)/2)*h
        t0 = t0+h
        y0 = Y(i)
    	IF (i==Ns) THEN
        	PRINT*,Y(i)
        END IF
	END DO
     
	DEALLOCATE(Y)
	   
	RETURN
    
    CONTAINS 
    	REAL FUNCTION dydt(t,y)
        	IMPLICIT NONE
        	REAL, INTENT(IN) :: t,y
        	dydt = y*t*t*t-1.5*y
        END FUNCTION dydt
               
END SUBROUTINE HeunsMethod

SUBROUTINE RalstonsMethod(y0,t0,Ns)
    REAL :: y0,t0,h,k1,k2,a1,a2
    INTEGER, INTENT(IN) :: Ns
    INTEGER :: i
    REAL, ALLOCATABLE, DIMENSION(:) :: Y
	h = 1.0/Ns
	a1 = 1.000/3.000
    	a2 = 2.000/3.000
	ALLOCATE(Y(Ns))
    
	DO i=1,Ns
    	k1=dydt(t0,y0)
        k2=dydt((t0+(3.000/4.000)*h),y0+(3.000/4.000)*h*k1)
   		Y(i) = y0 + (a1*k1+a2*k2)*h
        t0 = t0+h
        y0 = Y(i)
    	IF (i==Ns) THEN
        	PRINT*,Y(i)
        END IF
	END DO

	DEALLOCATE(Y)
	   
	RETURN
    
    CONTAINS 
    	REAL FUNCTION dydt(t,y)
        	IMPLICIT NONE
        	REAL, INTENT(IN) :: t,y
        	dydt = y*t*t*t-1.5*y
        END FUNCTION dydt
        
END SUBROUTINE RalstonsMethod

!yi+1=yi+1/6 *(k1,2k2,2k3,k4)h, k1=f(xi,yi),k2=f(xi+0.5h,yi+0.5k1h)
!k3=f(xi+0.5h,yi,0.5k2h, k4=f(xi+h,yi+k3h)

SUBROUTINE RungeKutta4(y0,t0,Ns)
    REAL :: y0,t0,h,k1,k2,k3,k4
    INTEGER, INTENT(IN) :: Ns
    INTEGER :: i
    REAL, ALLOCATABLE, DIMENSION(:) :: Y
	h = 1.0/Ns

	ALLOCATE(Y(Ns))
    
	DO i=1,Ns
    	k1=dydt(t0,y0)
        k2=dydt(t0+0.5*h,y0+0.5*k1*h)
        k3=dydt(t0+0.5*h,y0+0.5*k2*h)
        k4=dydt(t0+0.5*h,y0+k3*h)
   		Y(i) = y0 + (1.000/6.000)*(k1+2*k2+2*k3+k4)*h
        t0 = t0+h
        y0 = Y(i)
    	IF (i==Ns) THEN
        	PRINT*,Y(i)
        END IF
	END DO

	DEALLOCATE(Y)
	   
	RETURN
    
    CONTAINS 
    	REAL FUNCTION dydt(t,y)
        	IMPLICIT NONE
        	REAL, INTENT(IN) :: t,y
        	dydt = -25*y+0*t
        END FUNCTION dydt
        
END SUBROUTINE RungeKutta4

END MODULE ExplicitMethods
!_______________________________________________________
MODULE ImplicitMethods
CONTAINS
SUBROUTINE ImplicitEulerMethod(y0,t0,Ns)
    REAL :: y0,t0,h,yi,ti,yn
    INTEGER, INTENT(IN) :: Ns
    INTEGER :: i,iter
    REAL, ALLOCATABLE, DIMENSION(:) :: Y
	h = 1.0/Ns
	iter = 10
	ALLOCATE(Y(Ns))
    
	DO i=1,Ns
   		yi = y0 + h*(dydx(t0,y0))
        ti=t0+h
        yn=Newton(y0,ti,t0,h,iter)
        Y(i)=y0 + h*(dydx(ti,yn))
        t0 = ti
        y0 = Y(i)
    	IF (i==Ns) THEN
        	PRINT*,Y(i)
        END IF
	END DO
    
	DEALLOCATE(Y)
	   
	RETURN
    
    CONTAINS 
    	REAL FUNCTION dydx(x,y)
         	IMPLICIT NONE
      		REAL :: x,y
        	dydx = -21.6*y+0*x
       		END FUNCTION dydx
        
        REAL FUNCTION Newton(yk,tk1,tk,h,iter)
        	REAL, INTENT(IN) :: h
          	REAL :: yk,yk1,tk1,x1,gk1,gk 
            INTEGER :: iter,i
			yk1=yk+h*dydx(tk1,yk)
            DO i=1,iter
              gk1 = yk1-yk-h*dydx(tk1,yk1)
              gk=yk-h*dydx(tk1,yk1)
              x1 = yk1-(gk1*(yk-yk1))/(gk-gk1)
              yk1=x1
            END DO
            Newton = yk1
        END FUNCTION Newton      
   
END SUBROUTINE ImplicitEulerMethod

SUBROUTINE NSSHeunsMethod(y0,t0,Ns)
    REAL :: y0, t0,h,y1,yn,k1,k2
    INTEGER, INTENT(IN) :: Ns
    INTEGER :: i
    REAL, ALLOCATABLE, DIMENSION(:) :: Y
	h = 1/Ns
	PRINT*,'Input y_n-1'
    READ*, yn
    
	ALLOCATE(Y(Ns))
    
	DO i=1,Ns
    	k1=dydt(t0,y0)
        PRINT*,k1
        y1=yn+(k1*2*h)
        k2=dydt(t0+h,y1)
        Y(i) = y0 + ((k1+k2)/2)*h
        Y(i)=Y(i)-(Y(i)-y1)/5.0
        t0 = t0+h
        yn=y0
        y0 = Y(i)
    	IF (i==Ns) THEN
        	PRINT*,Y(i)
        END IF
	END DO
     
	DEALLOCATE(Y)
	   
	RETURN
    
    CONTAINS 
    	REAL FUNCTION dydt(t,y)
        	IMPLICIT NONE
        	REAL, INTENT(IN) :: t,y
        	dydt = 4*EXP(0.8*t)-0.5*y
        END FUNCTION dydt
        
END SUBROUTINE NSSHeunsMethod

END MODULE ImplicitMethods
!_______________________________________________________
PROGRAM example

    IMPLICIT NONE
    REAL :: yi,xi
    CHARACTER (LEN=10) :: s,m
    INTEGER :: N
	PRINT*,'Enter the initial parameters, y0, x0, N: '
    READ*, yi,xi,N
    PRINT*,'Implicit or Explicit method?'
    READ*, m
    IF (m == "Explicit") THEN
    	PRINT*,'Enter pick a method: Euler, Midpoint, Heuns, Ralstons, RK4 '
        READ*, s
    ELSE IF (m == "Implicit") THEN
      	PRINT*,'Enter pick a method: BackEuler, NSSHeuns '
        READ*, s
	ELSE
    	PRINT*,'Invalid response, default is Euler'
        s = 'Euler'
    END IF    
    
	CALL Choose(s,yi,xi,N)

END PROGRAM example
    

SUBROUTINE Choose(s,y,x,Nst)
	USE ExplicitMethods
    USE ImplicitMethods
	CHARACTER (LEN=*) :: s
    REAL :: y, x
    INTEGER :: Nst
    
	IF (s == 'Euler') THEN
    	CALL EulerMethod(y,x,Nst)
    ELSE IF (s == 'Heuns') THEN
    	CALL HeunsMethod(y,x,Nst)
    ELSE IF (s == 'Midpoint') THEN
    	CALL MidpointMethod(y,x,Nst)
    ELSE IF (s == 'Ralstons') THEN
      	CALL RalstonsMethod(y,x,Nst)
    ELSE IF (s == 'RK4') THEN
      	CALL RungeKutta4(y,x,Nst)
    ELSE IF (s == 'BackEuler') THEN
      	CALL ImplicitEulerMethod(y,x,Nst)
    ELSE IF (s == 'NSSHeuns') THEN
      	CALL NSSHeunsMethod(y,x,Nst)    
    ELSE
      	PRINT*,'You have entered an invalid method. Rerun.'
    END IF  
       		
END SUBROUTINE Choose  
