## The 2 functions permit to create a special object that stores a matrix and
## its inverse.

## The makeCacheMatrix() function returns a list of 4 fonctions
## and each of these functions returns a matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        
        ## 1. set() function permits to store 
        ##   the matrix that one want to compute inverse.
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        
        ## 2. get() function returns the initial matrix.
        get<-function(){x}
        
        ## 3. setInv() permits to sore the inverse in the Inv variable 
        ## that is in the closure of makeCacheMatrix().
        setInv<-function(m){
                inv<<-m
        }
        ## 4. getInv() returns the inverse of the initial matrix x.
        getInv<-function() {inv}
        list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## The function takes the input x (matrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Take the value of the inverse of x
        inv<-x$getInv()
        ## Check if inv is NULL or not
        if(!is.null(inv)){
                return(inv)
        }
        ## Here, we get the initial matrix, compute his inverse and store it
        mat<-x$get()
        inv<-solve(mat)
        x$setInv(inv)
        inv
}
