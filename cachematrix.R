
##This is a list of functions
makeInverse <- function(x = matrix()) {
  i <- NULL #Sets the inverse to NULL 
  set <- function(y) { 
    x <<- y ##Saves called matrix outside present environment
    i <<- NULL #Reset inverse to null every time called matrix is changed
  }
  get <- function() {x} ##Returns called matrix
  setsolve <- function(solve) {i <<- solve} ##Saves inverse outside present environment
  getsolve <- function() {i} ##Returns the Inverse 
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


cachesolve <- function(x, ...) {
  i <- x$getsolve() #Retrieves the inverse 
  if(!is.null(i)) {
    message("retrieving inverse")
    return(i) ##If i is not null then return

  }
  data <- x$get() #Called matrix is retrieved 
  i <- solve(data, ...) #Inverse is computed
  x$setsolve(i) #and cached in list
  i #return to user
}



