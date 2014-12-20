## Matrix inversion is usually a costly computation and they may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.  The pair of functions
## makeCacheMatrix and cacheSolve here will cache the inverse of a matrix.  

## The function of makeCacheMatrix creates a special "matrix" object that can cache its inverse form.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL       # m will be the result of solve() and it's reset to NULL every time
                  #makeCacheMatrix is called.
  
  
  set <- function(y) {     # take an input matrix
    x <<- y                # save the input matrix
    m <<- NULL             # reset the solve() result to NULL when a new object is generated.
  }  
  
  
  get <- function() x                       # return the value to the original matrix
  setsolve <- function(solve) m <<- solve   # this is called by slove() during the first solve()
                                            # access and it sill store the value using superassignment
  getsolve <- function() m                  # return the cached value using superassignment
  
  
  list(set = set, get = get,    
       setsolve = setsolve,    
       getsolve = getsolve)    # this function is accessed each time when makeCacheMatrix() is called.
                               # It generates a list of the internal functions to guide the calling
                               # function makeCacheMatrix() how to access the defined internal functions.
}                               


## The cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix()
## above.  If the inverse has already been calculted (and the matrix has not changed), then the 
## cacheSolve() should retrieve the inverse form  from the cache. 

cacheSolve <- function(x, ...) { # the input x is an object created by makeCacheMatrix
  
  m <- x$getsolve()              # access the object 'x" and gets the value of the solve
  
  if(!is.null(m)) {                  # if slove() was already cached (not NULL)...
    message("getting cached data")   # send the message to the console
    return(m)                        # return the inverse matrix from the cache
  }
  
  data <- x$get()                # if x$getsolve() returns NULL the input matrix is assigned to data
  m <- solve(data, ...)          # calculate its inverse 
  x$setsolve(m)                  # store the calculated inverse in x in makeCacheMatrix
  m                              # return the matrix that is the inverse of 'x'            
}
