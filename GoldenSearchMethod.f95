PROGRAM example

    IMPLICIT NONE
    INTEGER, PARAMETER  :: ikind=selected_real_kind(p=18)
    REAL (KIND=ikind):: x1,x2,STOPC,GoldenSearchMethod,maxAns

    PRINT*, 'Enter the start and end bounds and stop criteria'
    READ*, x1,x2,STOPC
    maxAns = GoldenSearchMethod(STOPC,x1,x2)
    PRINT*, "Root is:",maxAns

END PROGRAM example


REAL (KIND=selected_real_kind(p=18)) FUNCTION GoldenSearchMethod(SC,xO,xN)
    INTEGER, PARAMETER  :: ikind=selected_real_kind(p=18)
    REAL (KIND=ikind)::  d,X1,X2,xopt,GR,xO,xN
    REAL(KIND=ikind),INTENT(IN) :: SC
 	REAL (KIND=ikind), DIMENSION(1000) :: ea
    INTEGER :: numIts

   	ea(1) = 100
	numIts = 1
	GR = 0.61803

	DO WHILE(ea(numIts)>SC)
  		numIts=numIts+1
  		d=gr*(xN-xO)
  		X1 = xO +d
  		X2 = xN-d
  	IF(sin(X1)>sin(X2)) THEN
    	xO=X2
   		xopt = X1
  	ELSE IF(sin(X1)<sin(X2)) THEN
    	xN=X1
    	xopt = X2
  	END IF
  		ea(numIts)=(1-GR)*abs((xN-xO)/xopt)*100
	END DO
	GoldenSearchMethod = xopt

    RETURN
END FUNCTION GoldenSearchMethod



