angle <- function(x,y){
        dot.prod <- x%*%y 
        norm.x <- norm(x,type="2")
        norm.y <- norm(y,type="2")
        theta <- acos(dot.prod / (norm.x * norm.y))
        as.numeric(theta)
}

x <- as.matrix(c(sqrt(2), sqrt(2), 2))
y <- as.matrix(c(sqrt(2), sqrt(2), 0))
theta = angle(t(x),y) / pi 