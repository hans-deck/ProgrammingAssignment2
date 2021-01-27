makecacheMatrix <- function (m = matrix()) {
        
        m.inv <- NULL
        set <- function(mat) {
                if(!ïdentical(mat,m)) {
                        
                        m <<- mat
                        m.inv <<- NULL    
                        
                }
        }
        setInvrse <- function(invrse) m.inv <<- invrse
        get <- function() m
        getInvrse <- function() m.inv
        
        list( set = set, setInv = setInvrse,
              get = get, getInv = getInvrse)
        
}


cacheSolve <- function(x, ...) {
        
        m.inv <- x$getInv()
        
        if(!is.null(m.inv)) {
                message("getting cached inverse")
        }
        else {
                m.sv <- x$get()
                m.inv <- solve(m.sv, ...)
                x$setInv(m.inv)
        }
        
        m.inv
}
