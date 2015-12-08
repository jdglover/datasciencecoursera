makeCacheMatrix <- function(someMatrix = matrix()) {
    # Creates a list of functions that stores a matrix and caches its inverse.
    #
    # Args: 
    #    someMatrix: A matrix object of any size.
    # Returns:
    #    A list of functions setMatrix, getMatrix, setInversedMatrix,
    #    and getInversedMatrix.
    cachedMatrix <- NULL
    setMatrix <- function(y) {
        someMatrix <<- y
        cachedMatrix <<- NULL
    }
    getMatrix <- function() someMatrix
    setInversedMatrix <- function(inversedMatrix) 
        cachhedMatrix <<- inversedMatrix
    getInversedMatrix <- function() cachedMatrix
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInversedMatrix = setInversedMatrix,
         getInversedMatrix = getInversedMatrix)
}

cacheSolve <- function(makeCacheMatrixObject, ...) {
    # Calculates the inverse of a matrix and sets the value in the cache
    # if it not already cached. If the inverse is already cached, the 
    # function skips the calculation.
    # 
    # Args: 
    #    None
    # Returns:
    #    A a matrix that is the inverse of 'makeCacheMatrixObject'.
    inversedMatrix <- makeCacheMatrixObject$getInversedMatrix()
    if(!is.null(inversedMatrix)) {
        message("getting cached matrix")
        return(inversedMatrix)
    }
    aMatrix <- makeCacheMatrixObject$getMatrix()
    inversedMatrix <- solve(aMatrix)
    makeCacheMatrixObject$setInversedMatrix(inversedMatrix)
    inversedMatrix
}
