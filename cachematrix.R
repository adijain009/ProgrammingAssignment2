## The following matrix will compte the inverse of a matrix and stores it in a cache.

## makeCacheMatrix is going to make a  matrix, and will returns a list of subfunctions either 
##  setting  value of the matrix,getting  current value of the matrix,getting inverse of the matrix,getting  inverse of the matrix

makeCacheMatrix <- function(y = matrix()) {
       inv <- NULL
        set <- function(z) {
        y <<- z
        inv <<- NULL
    }
       get <- function() y
       setinv <- function(inverse) inv <<- inverse 
       getinv <- function() inv
            list(set = set, get = get,
              setinv = setinv,
                 getinv = getinv)
}


## cacheSolve checks  if an inverse of the matrix is already  computed and cached. 
### If yes, it returns the cached value. Otherwise, it computes the inverse.
cacheSolve <- function(y, ...) {
        ## Return matix inverse of 'y'
          inv <- y$getinv()
          if(!is.null(inv)) {
             message("getting cached data")
             return(inv)
    }
       data <- y$get()
       inv <- solve(data, ...)
        y$setinv(inv)
         inv
}

### to test the code
b=makeCacheMatrix(matrix(c(1,0,0,1),2,2))  ### getting the special matrix
cacheSolve(b)  ### calculating the inverse
b$getinv()  ### checking that the inverse in cache is the one calculated above
b$setinv(4)  ## setting inverse to 4
cacheSolve(b) ### should return 4
