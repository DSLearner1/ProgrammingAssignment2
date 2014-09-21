## The below pair of functions cache the inverse of a matrix 
## 
## The first function ,"makeCacheMatrix" creates a special matrix,which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s<-NULL
  set<-function(y){
    x<<-y
    s<<-NULL
  }
  get<-function()x
  setmean<-function(solve) s<<-solve
  getmean<-function()s
  
  list(set=set,get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## The below function ,"cacheSolve" calculates the inverse of the special matrix created with the above function"makeMatrix".It checks to see if the inverse has already been calculated.If so it gets the inverse from cache and skips calculation.If not it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s<-x$getmatrix()
  if(!is.null(s)){
    message("getting cached data")
    return (s)
  }
  data<-x$get()
  s<-solve(data,...)
  x$setmatrix(s)
  s

 
}
