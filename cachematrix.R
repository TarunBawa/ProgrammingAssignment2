## These functions will help to cache  the inverse of a matrix
## 

## makeCacheMatrix will create a special matrix which help us to cache the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

  
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }


    get <- function() x

    #  set the inverse
    setinverse <- function(inverse) inv <<- inverse

    # Get the inverse
    getinverse <- function() inv

    # Encapsulate into a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)	
}



## This Function returns the inverse of the matrix

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()

    # If it is 0
    if(!is.null(inv)) {
    	#  Return the computed inverse		
        message("Getting cached matrix")
        return(inv)
    }

    # If it hasn't...
    # Get the matrix itself
    data <- x$get()

    # Find the inverse
    inv <- solve(data, ...)

    # Cache this result in the object
    x$setinverse(inv)

    # Return this new result
    inv    
}
