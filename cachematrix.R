# The functions makeMatrix and cacheInverse take a matrix class object and return its inverse.
# The first time the inverse is calculated it is cached and if subsequently called it's returned from cacheInverse
# foregoing recalculation.

# Input is matrix. Subfunctions are list called from cacheInverse. 
# use x$get - display input values.
# use x$set - input new values for matrix. 


makeMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() {
		x
		}
        setInverse <- function(solve) {
		I <<- solve
		}
        getInverse <- function() {
		I
		}
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# cacheInverse returns inverse of matrix specified in makeMatrix
# Retrieves matrix input directly, calculates and displays inverse, and updates cache with that value; but prior...
# Checks if inverse already calculated and if so retrieves and displays cached value without executing calculation.
cacheInverse <- function(x, ...) {
        I <- x$getInverse()
		
			if(!is.null(I)) {
					message("getting cached data")
					return(I)
			}
			
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        I
}