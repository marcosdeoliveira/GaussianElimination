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
# -- matrixTestA:
A = matrix(
  c(0.5,-1,1,6,
    3,2,1,8,
    5,-1,-3,-1),
  3,4, TRUE  # matrixLines, matrixColumns, lineFirst
)
# -- matrixTestC:
# C = matrix(
#       c(3,1,1,20,
#       2,-1,-1,-15,
#       -4,1,-5,-41),
#       3,4, TRUE
#    )

# -- constants:
strLine = "line"
strCol = "column"

toTriangular<-function(M){

  matrixLines= length(M[,1])    # length of first column in M
  matrixColumns = length(M[1,]) #length of first line in M

  # -- lines iteration
  for (i in 1:matrixLines){

    if(i==1){   # do nothing in first line
      next
    }

    # -- columns iteration
    for(j in 1:matrixColumns){

      if(i<=j){ # -- it's not a pivot
        next
      }

      # -- it's a pivot:
      pivot = M[i,j]/M[j,j]

      for(k in j:length(M[1,])){    #  calculate each column for actual pivot
        M[i,k]= M[i,k]-pivot*M[j,k]
      }

      # iteration debug:
      # strAux = paste(strLine,i,",",strCol,j)
      # print(strAux)
      # print(paste("pivot",pivot))
      # print(M);

    }
  }

  return(M)
}
