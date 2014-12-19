##R Programming Course on Coursera, Assignment 2

makeCacheMatrix<-function(x=matrix()) {
            if (nrow(x)!=ncol(x)) stop ("x is not a square matrix")      
            #Since the non-square matrix fault would not be resolved before step 2 (cacheSolve function)
            #I've put this error message at the beginning.
            Inverse<-NULL
            setMatrix<-function(y) {
                  x<<-y
                  Inverse<<-NULL
            }
                  getMatrix<-function() x
                  setInverse<-function(solve) Inverse<<-solve(x)
                  getInverse<-function() Inverse
                        list(setMatrix=setMatrix,
                        getMatrix=getMatrix,
                        setInverse=setInverse,
                        getInverse=getInverse)
}

cacheSolve<-function(x,...) {
      Inverse<-x$getInverse()
            if(!is.null(Inverse)) {
                  message ("getting cached data")
                  return(Inverse)
      }
                  data<-x$getMatrix()
                  Inverse<-solve(data)
                  x$setInverse(Inverse,...)
            Inverse
}

#basic testing
A<-matrix(rnorm(25),5,5)
m<-makeCacheMatrix(A)
B<-cacheSolve(m)

round(A%*%B, digits=0)#This should yield an identity matrix, with 1's on
#its diagonal, and otherwise zeros
