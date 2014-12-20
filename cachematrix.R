### Functions makeCacheMatrix and cacheSolve can be used together to calculate
### and cache the inverse of a matrix.

## Function makeCacheMatrix creates a matrix object that can cache its inverse.
## 		Input x is a matrix. 
##		Output is a list that stores input + cached value of x.
makeCacheMatrix <- function(x = matrix()) {
        # Object inv will be inverse matrix, set to NULL w/ each function call.
        inv <- NULL
        
        # Function set
        #       Input (y) is a matrix
        set <- function(y) {
                
                # Input matrix y is saved to x.
                x <<- y
                
                # Inverse matrix is reset to null.
                inv <<- NULL
        }
        
        # Object get returns value of original matrix to cacheSolve().
        get <- function() { x }
        
        # Object setinv is called by cacheSolve() during first access
        # and stores value using superassignment.
        setinv <- function(inverse) { inv <<- inverse }
        
        # Object getinv returns cached value to cacheSolve().
        getinv <- function() { inv } 
        
        # List is accessed w/ each function call.
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Function cacheSolve
##		Input x is list created by makeCacheMatrix.
##      Output is either cached or newly calculated inverse matrix.
cacheSolve <- function(x, ...) {
        # Object inv extracts inverse matrix element of input list x.
        inv <- x$getinv()
        
        # If that inverse matrix was already cached (i.e., is not null)...
        if (!is.null(inv)) {
                
                # then send message to console...
                message("Retrieving cached data!")
                
                # and return inverse matrix, thereby ending function.
                return(inv)
        }
        
        # If inverse matrix was not already cached, access original matrix...
        mtrx <- x$get()
        
        # calculate the inverse...
        inv <- solve(mtrx)
        
        # store the calculated inverse in the CacheMatrix list...
        x$setinv(inv)
        
        # and return the inverse matrix.
        inv
}
