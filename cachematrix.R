makeCacheMatrix <- function(x = matrix()) { ## to set the value of the matrix
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x  ## to get the value of the matrix
        setmatrix<-function(solve) m<<- solve ## to set the inverse of the matrix
        getmatrix<-function() m
        list(set=set, get=get, ## to get the inverse
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) { ## to return inverse of matrix
        m<-x$getmatrix()   ## to get the inverse of matrix
        if(!is.null(m)){    ## to check if there is a matrix
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()   ## if not, get inverse of matrix
        m<-solve(matrix, ...)
        x$setmatrix(m)    ## set the inverse of matrix
        m
}
