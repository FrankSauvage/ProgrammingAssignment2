## The following pair of functions allow to cache the potentially time-consuming matrix inversion. 
## The inversion will not be computed again if it has been computed once and if the original matrix doesn't change since then.
## In the previous cases, the inverse matrix will be simply accessed and returned.
## If changes occured in the matrix or if no inverse has been calculated yet, the "cacheSolve" function computes it and assign it in the 
## object assigned by makeCacheMatrix().

## The first function "makeCacheMatrix" creates an object that get a matrix and which can also cache its inverse.
## e.g. yy <- makeCacheMatrix(matrix(c(seq(1,8),0),3,3, byrow=T)) creates an object yy containing the matrix and initializing
## the inv variable to NULL in the makeCacheMatrix() function's environment associated to the object yy.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #initialize the inverse matrix to NULL
        set <- function(mat) { #If the object receives a new assignment, the set() function update the matrix (in x) and reinitialize the inverse inv to NULL.
                x <<- mat
                inv <<- NULL
        }
        get <- function() x #the function to get the value of the non-inverted matrix
        setinv <- function(inv.mat) inv <<- inv.mat #the function to update the inverse matrix "inv" using the value of "inv.mat"
        getinv <- function() inv #the function to get the value of "inv", hence NULL or the inverse matrix computed by cacheSolve() and cached in "inv"
        list(set = set, get = get, #the output of the makeCacheMatrix() function that cacheSolve() will use.
             setinv = setinv,
             getinv = getinv)
}


## The second function "cacheSolve" checks for an accurate inverse value in "makeCacheMatrix" or computes it if required.
## If we call cacheSolve(yy), then When inv <- x$getinv() is read in cacheSolve function, R searches for the value of inv 
## in the getinv() defining environment. This means it searches the local makeCacheMatrix() environment.
## As a result, inv in cacheSolve(yy) receive the value of yy$getinv(). This value is NULL at first call but will remains
## the value of solve(yy$get()) afterwards, as long as no new assignation affects yy.
 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() #get the value of "inv" in the object created by makeCacheMatrix()
        if (!is.null(inv)){ #test the returned value of "inv": if different from NULL, it returns the value of "inv" and a message informs the result comes from the cache
                message("Getting cached inverse")
                return(inv) #as the return() function is used, this stop the cacheSolve() function and returns "inv"
        }
        data <- x$get() #will be executed only if inv==NULL, it gets the value of the original matrix in the object created by makeCacheMatrix() and stores it in "data"
        inv <- solve(data, ...) #computes the inverse of "data"
        x$setinv(inv) #set the value of "inv" in the object created by makeCacheMatrix() to the value of "inv" computed the line above
        inv #returns the value of the inverse matrix, without the message "Getting cached inverse"
}

# Proposed code to test the functions and how they work
# The results of the following line reveals that the second cacheSolve(yy) (and the following attempts) call get the cached inverse matrix.
# yy <- makeCacheMatrix(matrix(c(seq(1,8),0),3,3, byrow=T)); cacheSolve(yy); cacheSolve(yy)

# The result of the following line reveals that the second call cacheSolve(zz) recomputes the inverse as the first assigns zz, even if values are the same.
# cacheSolve(zz <- makeCacheMatrix(matrix(c(seq(11,18),0),3,3, byrow=T))); cacheSolve(zz)
# zz <- matrix(c(seq(1,8),0),3,3, byrow=T); det(zz); solve(zz); 

# The object created by makeCacheMatrix contains the original matrix, its inverse and the function to set and get them. E.g.:
# yy$getinv(); zz$getinv()