A <- 4
B <- 5

N <- 10000
random.sampling.1 <- rbeta(N, A, B)

random.sampling.2 <- c()
position <- 0.5
STEP <- 0.006
for(index in seq(1, N)) {
    proposedPosition <- sample(c(position - STEP, position + STEP), 1)
    probablilityToMove <- if( proposedPosition < 0 ) {
        0
    } else if( proposedPosition > 1 ) {
        0
    } else {
        currentDepsity <- dbeta(position, A, B)
        proposedDensity <- dbeta(proposedPosition, A, B)
        proposedDensity / currentDepsity
    }

    if( probablilityToMove >= runif( 1 ) ) {
        position <- proposedPosition
    }
    random.sampling.2 <- c(random.sampling.2, position)
}

par(mfrow=(c(1, 2)))
hist(random.sampling.1)
hist(random.sampling.2)


