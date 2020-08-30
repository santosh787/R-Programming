## function creates a special matrix 

makeCacheMatrix <- function(a = matrix()) {
  #set a null matrix
  inv<-NULL
  set<-function(a){
    #super-assignment of values in global environment
    a<<-b
    inv<<-NULL
  }
  get<-function()a
  calcinverse<-function(inverse) inv<<-inverse
  retinverse<-function() inv
  list(set=set,get=get,calcinverse=calcinverse, retinverse=retinverse)
}


## function computes inverse of special matrix

cacheSolve <- function(x, ...) {
  inv <- x$retinvers()
  if (!is.null(inv))
  {
    message("inverse already computed; getting cached data")
    return(inv)
  }
        data<-x$get()
        inv<-solve(data,...)
        x$calcinverse(inv)
        return(inv)
}
