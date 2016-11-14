#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#
#
# Before use this program you need create and store your matrix.
#
# This one just gets one input matrix (of a linar system), and return
# an vector with the solution.
# There's messages of debugs desabled inside the secundary functions.
#

gaussianElimin<-function(storedMatrixtoSolve){
  #turn the niputed matrix in a triangular matrix
  triangularMatrix = toTriangular(storedMatrixtoSolve)
  # # -- debug
  # print("triangularMatrix: ")
  # print(triangularMatrix)

  # solves the linear system
  solvedSystem = solveSystem(triangularMatrix)
  # # -- debug
  # print("solvedSystem: ")
  # print(solvedSystem)

  return(solvedSystem)

}
