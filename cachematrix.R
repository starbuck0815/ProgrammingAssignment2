## The purpose of these functions is to calcualte and cache the inverse of an 
## input matrix. If the result has been calculated previously, it should be 
## retrieved from cache instead of calculated again, potentially reducing 
## computing time for large input matrices.

## makeCacheMatrix takes an input matrix and generates an output object, 
## which is a list of four functions (get matrix, set matrix, get inverse of 
## matrix, set inverse of matrix) and can cache its inverse. This object serves 
## as input to the "cacheSolve" function where the actual calculation is 
## carried out
## 

makeCacheMatrix <- function(x = matrix()) {
        #initialize
        inv <- NULL
        
        # set function: if new matrix is set, previously cached results
        # are set to NULL, so that no outdated results can be retrieved
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #get function to retrieve input matrix
        get <- function() x
        #function to store calculated inverse of matrix to cache
        setinv <- function(inverse) inv <<- inverse
        #function to read cached result
        getinv <- function() inv
        #list returned (output object)
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## "cacheSolve" reads the object created by "makeCacheMatrix" for the input
## matrix, calculates the inverse of the matrix and stores the result in the
## cache. If the result already exists in the cache, it is read and returned
## without calculation.

## for this assignment, it is assumed that all input matrices are invertible,
## therefore the properties of the matrix are not explicitly checked.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #retrieve cached inverse (or NULL)
        inv <- x$getinv()
        #if cached result exists, return the inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #else get the matrix and calculate the inverse via "solve"
        data <- x$get()
        inv <- solve(data)
        #store result in cache
        x$setinv(inv)
        #return the inverse
        inv
        
}
