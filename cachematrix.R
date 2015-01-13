## This function is used to calculate matrix inverse. The computed inverse values are cached.
## The cached inverse is retrived whenever is asked for(given the matrix has not changed).
## A new invese is calculated and cached(if the matrix has changed).

## Example:-
## x<-makeCacheMatrix(); # create cachematrix object
## x$set(matrix(c(0,1,2,3),nrow=2,ncol=2)); # will provide a matrix to the catchmatrix object
## cacheSolve(x); # will calculate and cache the inverse of the matrix
## cacheSolve(x); # will retrive the previously stored inverse of the matrix

## Creates a matrix object which can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL;
  set<-function(y){ x<<-y; inv<<-NULL;}
  get<-function() {x;}
  setinv<-function(z){inv<<-z;}
  getinv<-function(){inv;}
  return(list(set=set,get=get,setinv=setinv,getinv=getinv));
}

## Calculates the inverse of a matrix or returns the cached inverse of the matrix
cacheSolve <- function(x, ...) {
  inv<-x$getinv();
  if(!is.null(inv)) {
    message("getting cached data");
    return(inv);
  }
  data<-x$get();
  inv<-solve(data);
  x$setinv(inv);
  return(inv);
}
