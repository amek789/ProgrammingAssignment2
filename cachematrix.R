## Below are two functions that are used to create a special object that stores a matrix,
##calculates its inverse and caches it.

##The first function, makeCacheMatrix creates a special Matrix, which is really a list containing functions to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


##The following function calculates the inverse of the matrix 
##created with the above function. However, it first checks to see if
##the inverse has already been calculated. If so, it gets the inverse from 
##the cache and skips the computation. Otherwise, it calculates the 
##inverse of the given matrix and sets the value of the inverse in the cache via 
##the setmatrix function.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix)
  x$setmatrix(m)
  m
}


##Sample run: 
##myMatrix<-makeCacheMatrix()
##> myMatrix$set(matrix(runif(2*2),ncol = 2))
##> cacheSolve(myMatrix)
##           [,1]      [,2]
##[1,]  3.1517475 -2.638948
##[2,] -0.7181506  1.707706
##> cacheSolve(myMatrix)
##getting cached data
##           [,1]      [,2]
##[1,]  3.1517475 -2.638948
##[2,] -0.7181506  1.707706
##> newMatrix<-makeCacheMatrix()
##> newMatrix$set(matrix(runif(3*3),ncol = 3))
##> cacheSolve(newMatrix)
##           [,1]       [,2]       [,3]
##[1,]  -2.856099  0.2464152   4.604486
##[2,] -12.201384  5.6881321  12.313313
##[3,]  15.466994 -5.3345775 -16.059511
##> 
