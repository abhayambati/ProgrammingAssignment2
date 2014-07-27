## The set of two functions cache the inverse of a square matrix 
## and provide the inverse from the cache (when called upon) without computing it again.

## makeCacheMatrix() stores the matrix, and its inverse from the cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
                      i <- NULL
                      set <- function(y){
                        x <<- y
                        i <<- NULL  
                      }
                      get <- function() x
                      setinv <- function(solve) i <<- solve
                      getinv <- function() i
                      list(set = set, get = get,
                           setinv = setinv,
                           getinv = getinv)

                    }


## cacheSolve() generates inverse of the matrix x and stores it in makeCacheMatrix()
## if there is no inverse already generated for the matrix x

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i ## Return a matrix that is the inverse of 'x'
        
}
