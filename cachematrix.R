## set - stores a matrix
## get - returns the stored matrix
## setcache - sets the cache as argument
## getcache - returns the previuously set cache
## cacheSolve - calculates the inverse of a matrix, only after checking if it was already calculated and returning
##              it from cache with the message "getting cached data"


## This function allows you to store a matrix, to return the matrix, to set the cache and in the end return it.

makeCacheMatrix <- function(x = numeric()) {
        # holds the cached value or NULL if nothing is cached
        # nothing is cached initially so we set it to NULL
        m <- NULL
        
        #Store a matrix
        set <- function(y) {
                x <<- y
                #A new value is atribuited to the matrix, so we flush the cache
                m <<- NULL
        }
        #Return the previously stored matrix
        get <- function() {
                x
        }
        #Set the cache as given argument
        setcache <- function(solve) {
                m <<- solve
        }
        #Return the cache set previously
        getcache <- function() {
                m
        }
        #Return a list in which each named element is a function
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}


## This function calculates the inverse of a matrix, but only after checking if the inverse has already been calculated
## and if so, returns it from cache.

cacheSolve <- function(x, ...) {
                
                #Atribuite cache value to m
                m <- x$getcache()
                
                #If cache is not NULL, print the message"getting cached data" and return the value m from cache
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                
                #Atribuite the value of the stored matrix to data
                data <- x$get()
                
                #Return the inverse of data and atribuite it to m
                m <- solve(data)
                
                #Set the cache as the inverse of m, previously calculated, so it will be cached for subsequent use
                x$setcache(m)
                
                #Return the inverse of a matrix
                m
}
