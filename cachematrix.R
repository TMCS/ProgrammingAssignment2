## This function creates a special "matrix" object 
## that pushes the resulting computation of inversing
## a matrix to cache memory

## Function description
## The function argrument x is a square matrix
## They are 6 parts to this function, four sub function that get return 
## in a list by makeCacheMatrix().
## Note: No inverse computation carried out when calling makeCacheMatrix()
## there are 6 parts to the function   
##    1. declaring local m vector as matrix (empty matrix)
##	2. set() function copies x to cache and clears the cache
##	   variable m
##	3. get() returns x, the matrix to be computed
##	4. setinv() computes the inverse of the matrix and assigns the result 
##	   to m in cache 
##	5. getinv() retrieves m from cache
##	6. the four functions are returned in a list to be use as agruments for
##       the cachesolve function 



makeCacheMatrix <- function(x = matrix()) {
	
        m <- matrix() 
        set <- function(y) {
                x <<- y
                m <<- matrix()
		  
        }

        get <- function() x
        setinv <- function(solve) m <<- solve 
        getinv <- function() m
        list(set = set, get = get,      ## return the 4 functions in a list
             setinverse = setinv,
             getinverse = getinv)

}



## cacheSolve function
## The output list of functions from the makeCacheMatrix()
## form the input agruments for this function.
## m variable is read from cache. It will either the inverse of 
## a previous inverse computation or a an empty matrix
## If m is not an empty matix, it will have numeric value in row1, col1,
## i.e m contain a previous computation of matrix. The user is notified that 
## result is being retrieved from cache. 
## if matrix in cache is empty, the inverse of the matrix is computated and
## stored in cache with setinverse
 

cacheSolve <- function(x, ...) {
       m <- x$getinverse()        ## reading Cache 
       if(is.numeric(m[1,1])) {   ## matrix data check,
       message("getting cached data")
       return(m)
      }
       data <- x$get()          ## get matrix for computation
       m <- solve(data, ...)    ##compute inverse of matrix
       x$setinverse(m)  	## store the result in cache 
	m  			## return result, inverse of x

}
