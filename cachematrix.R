## Matrix inversion can be a time consuming process
##for large matrix size. So here we write a function to cache 
##the inverse of the matrix and return it if the original matrix 
##hasn't changed
 

## makeCacheMatrix function creates a special 
## "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL 				# Define function to set the value of the matrix. 
    		  				# It also clears  the old inverse from the cache
    set <- function(y) 
		{
        	x <<- y 			# Set the value
            m <<- NULL			# Clear the cache
		}
    get <- function()x 			# Define function to get the value of the matrix
    setInverse <- function(inverse) # Define function to set the inverse. This is only used by getinverse() when
   						# there is no cached inverse

    m <<- inverse				
    getInverse <- function() m	# Define function to get the inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)	# Return a list with the above four functions
}


## cacheSolve: This function computes the inverse of
## the special "matrix" returned by makeCacheMatrix.
## if the inverse is already stored in cache and source
## matrix has not changes then it will return data stored in cache 

cacheSolve <- function(x) 
{
	 m <- x$getInverse() 		# This fetches the cached value for the inverse
	    if(!is.null(m)) 		# If the cache was not empty, we can just return it
		{ 
	        message("Getting cached data")
      	  return(m)
		}
						# The cache was empty. We need to calculate it, cache it, and then return it.
	data <- x$get()      		# Get value of matrix
	m <- solve(data) 			# Calculate inverse
	x$setInverse(m)			# Cache the result
	m           			# Return the inverse
}