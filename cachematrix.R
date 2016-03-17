#Barmil Tanak
#18 Mar 2016
#Programming Assignemnmt 2 Week 3

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x =matrix())  {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inversematrix) m <<- inversematrix
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}

cacheSolve <- function(x, ...) {
  m <- x$getinversematrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<- solve(data, ...)
  x$setinversematrix(m)
  m
}

#The below Code is to test the tabove functions:

message("Create an invertible Matrix")
myMat<- matrix(c(-1,-2,-2,-1,2,0,2,-1,4),nrow=3, ncol=3,byrow=TRUE, dimnames = list(c("R1","R2","R3"),c("C1","C2","C3")))
myMat
message("Test if it is invertible")
solve(myMat)
message("Create the Special Matrix")
specialmat<-makeCacheMatrix()
specialmat$set(myMat)
message("Compute the inverse 1st time: no Cache It should compute the inverse")
cacheSolve(specialmat)
message("Compute the inverse 2nd time: Inverse retreived from the Cache")
cacheSolve(specialmat)