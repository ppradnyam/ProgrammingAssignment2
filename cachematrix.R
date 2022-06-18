makeCacheMatrix <- function(mat) {
        m <- NULL
        set <- function(y) {
                mat <<- y
                m <<- NULL
        }
        get <- function() mat
        setrev <- function(rev) m <<- rev
        getrev <- function() m
        list(set = set, get = get,
             setrev = setrev,
             getrev = getrev)
}


cacheSolve <- function(mat, ...) {
        m <- mat$getrev()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- mat$get()
        m <- solve(data, ...)
        mat$setrev(m)
        m
}
