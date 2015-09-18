## Create 2 functions to create a special object to store and cache 
## inverse of a matrix

## Create a special "vector" which has 4 functions, 1
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL ## value of matrix inverse
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  
  get <- function(){x}
  
  setInvMatrix <- function(Inv.Mat){m<<-Inv.Mat}
  
  getInvMatrix <- function(){m}
  
  list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## calculates inverse matrix of the "special" vector, 
## if the value has not been cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getInvMatrix()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setInvMatrix(m)
  
}
