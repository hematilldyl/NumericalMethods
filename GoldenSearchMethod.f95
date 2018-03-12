PROGRAM example
	
	IMPLICIT NONE
    REAL  :: x1,x2,STOPC,GoldenSearchMethod,maxAns
    x1 = 0
    x2=1
    STOPC = 1
    maxAns = GoldenSearchMethod(STOPC,x1,x2)
    PRINT*, "Root is:",maxAns

END PROGRAM example
    

REAL FUNCTION GoldenSearchMethod(SC,xO,xN)

    REAL ::  d,X1,X2,xopt,GR,xO,xN
    REAL, INTENT(IN) :: SC
    
	REAL, DIMENSION(100) :: ea
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
    	xN=X2
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


