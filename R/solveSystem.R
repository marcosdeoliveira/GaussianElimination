#
# -- matrixTestB
# 0.5   -1  1.000   6.0
# 0.0    8 -5.000 -28.0
# 0.0    0 -7.375 -29.5
# -- matrixTestD
# 3  1.000000  1.000000  20.00000
# 0 -1.666667 -1.666667 -28.33333
# 0  0.000000 -6.000000 -54.00000
#

# -- constants:
strLine = "line"
strCol = "column"

solveSystem<-function(M){

  matrixLines= length(M[,1])    # length of first column in M
  matrixColumns = length(M[1,]) # length of first line in M

  numVar = matrixLines       # number of variables on system
  indepTerm = matrixColumns  # column of independent terms

  # -- create and start fill each element in output vector with zero
  outVector= c(1: numVar)
  for(el in outVector){
    outVector[el]=0
  }

  # -- iterate lines in matrix from down to up
  for(var in numVar:1){

    # store the independent term
    outVector[var] = M[var,indepTerm]

    if(var>=numVar){ # solve the last line
      outVector[var] = outVector[var]/M[var,var]
      next
    }

    # -- iterate column in matrix from right to left
    for(i in numVar:(var+1)){

      outVector[var] = outVector[var] - M[var,i]*outVector[i]

      # iteration debug:
      # strAux = paste(strLine,var,",",strCol,i)
      # print(strAux)
    }

    outVector[var] = outVector[var]/M[var,var]

    # iteration output debug:
    # print(outVector)
  }

  return(matrix(outVector,numVar,1,FALSE))

}
