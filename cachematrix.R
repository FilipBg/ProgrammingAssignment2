## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Same principles apply as in given example. I is in inverse set to
## null for new matrix or when changing values using set function.
## set,get,setinv,getinv are function definitions returned as a list in last row.
makeCacheMatrix <- function(x = matrix()) {
  I<- NULL
  
  set<- function(y){
    x<<-y
    I<<-NULL}
  get<- function(){x}
  setinv<-function(inv) {I<<- inv}
  getinv<-function() {I}
  
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
## extracts inverse from list of functions (getinv) and checks
## if it is already calculated (if loop)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getinv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinv(I)
  I
}
