
## Creates a matrix and then calcualtes it's inverse. It then caches the result

makeCacheMatrix <- function(x = matrix()) {
  ##initialize the cache(variable)
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  ##stores result
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


##checks to see if the inverse has already been calculated and cached
##if so it returns the value in cache
##if not it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  ##looks to see if it is already calcualted 
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ##if not it does the calculation
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
