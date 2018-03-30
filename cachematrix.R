## cacheSolve function will calculate the inverse of matrix & save it in m
## if it is NULL, otherwise will be return the m already saved in the 
## makeCacheMAtrix function. m will be resetted to NULL 
## everytime new values are included as input to X matrix 

## makeCacheMatrix
## Argument X is initiallized as matrix
## setMatrix - set x & m value in parent environment
## getMatrix - get x matrix from the parent environment
## setInverseMatrix - saves the inverse matrix once calculated
## getInverseMatrix - get the saved inverse matrix output
## lists all the function created

makeCacheMatrix <- function(x = matrix()) {
  m<<-NULL
  setMatrix<-function(y){
    x<<-y
    m<<-NULL
  }
  getMatrix<-function(){
    x
  }
  setInverseMatrix<-function(Matrix){
    m<<-Matrix
  }
  getInverseMatrix<-function(){
    m
  }
  list(setMatrix=setMatrix,getMatrix=getMatrix,setInverseMatrix=setInverseMatrix,getInverseMatrix=getInverseMatrix)

}


## cacheSolve function
## gets the x matrix from makeCacheMatrix
## Checks if m is null or not
## if null it will calculate the inverse of matrix
## otherwise it will retrun the inverse of matrix already saved

cacheSolve <- function(x, ...) {
  m<-x$getInverseMatrix()
  if(!is.null(m)){
    message("getting cached matrix")
    return(m)
  }
  data<-x$getMatrix()
  m<-solve(data,...)
  x$setInverseMatrix(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
