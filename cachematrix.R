##***********************************************************************************************
##***********************************************************************************************
##*                                                                                             *
##*            Filename: cachematrix.R                                                          *
##*  Included Functions: makeCacheMatrix                                                        *
##*                      cacheSolve                                                             *       
##*        Date Written: Saturday, June 21, 2014                                                *
##*              Author: Brian Farish                                                           *
##*         Description: This file implements two functions that demonstrate the following:     *
##*                      1) calculating the inverse of a matrix using "solve"                   *
##*                      2) storing the results of a calculation in "cache" for reuse           *
##*                      3) retrieving the cached value and displaying it                       *
##*                      4) detecting that the calculation has already been performed on        *
##*                         subsequent attempts to run the calculation and returning the        *
##*                         cached value instead of re-running the calculation.                 *
##*                                                                                             *
##***********************************************************************************************
##***********************************************************************************************

##***********************************************************************************************
##*                                                                                             *
##* makeCacheMatrix takes supplied parameters and builds a matrix which is stored in cache.     *
##* It also defines methods that can be used to access the results and manipulate it.           *
##*                                                                                             *
##***********************************************************************************************

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setCacheMatrix <- function(solve) m <<- solve
        getCacheMatrix <- function() m
        list(set = set, get = get,
             setCacheMatrix = setCacheMatrix,
             getCacheMatrix = getCacheMatrix)
}

##***********************************************************************************************
##*                                                                                             *
##* cacheSolve checks to see if a result has been previously cached and if so, returns that     *
##* value.  If not, it does the matrix inversion and returns the value and caches it.           *
##*                                                                                             *
##***********************************************************************************************

cacheSolve <- function(x = matrix(), ...) {
        ## 
        ## Return a matrix that is the inverse of 'x'
        ## Refers to a method previously defined in makeCacheMatrix to retrieve the cached value 
        ## 
              m <- x$getCacheMatrix()
              ## Check to see if m has been defined and if so, retrieve that value and return it.
              if(!is.null(m)) {
                      message("getting cached data")
                      return(solve(m))
              }
              ## If no value was detected for m, do the calculation and return the inverted
              ## matrix.
              data <- x$get()
              m <- solve(data, ...)
              x$setCacheMatrix(m)
              m
}
