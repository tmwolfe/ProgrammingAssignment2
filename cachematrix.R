## The first function creates a matrix object that can cache its inverse. The second function
## computes the inverse of the matrix returned by makeCacheMatrix above. If the matrix has 
## already been calculated then it retrieves the inverse from the cache, otherwise it will
## calculate the inverse. Assume that the matrix is always invertible.

## The function below creates a special matrix where x is the invertible matrix and it  
## returns a list that sets the matrix, gets the matrix, sets the inverse, and gets the
## inverse. The list will be used later in the second function cacheSolve(). <<- operator
## assigns the value on the right side of the operator to an object in the parent environment
## named by the object on the left side of the operator. See breakdown below.

makeCacheMatrix <- function(x = matrix()) {
        ## Create matrix
        inv <- NULL
        ## Assigns the input argument to the x object in the parent environment and the Null
        ## value to the inv object in the parent environment.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## Defines the getter of the matrix x. X is not defined in the function so it is 
        ##retrieved from the parent environment.
        get <- function() x
        ## Defines the setter. Assigns the input argument to the value of m in parent 
        ## environment
        setinverse <- function(inverse) inv <<- inverse
        ## Defines the getter for m
        getinverse <- function() inv
        ## Returns a list to the parent environment
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the matrix x which is returned by makeCacheMatrix 
## above. If the matrix has already been calculated then it retrieves the inverse from the 
## cache. Otherwise it will calculate the inverse. See below for breakdown.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        ## If calculated already then retrieve it from cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Otherwise calculate the inverse and set value in the cache
        data <- x$get()
        inv <- inverse(data, ...)
        x$setinverse(inv)
        inv
}

