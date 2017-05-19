particleSwarm = function(f, popSize=10, d=2, l.bound=0, u.bound=1, w, c1, c2, maxIter=100, criterion=FALSE)
{
  # psoAlgorithm
  # INPUT
  #      - f objective function
  #      - popSize number of particles 
  #      - d number of variables
  #      - l.bound lower boundary (for initial particles)
  #      - u.bound upper boundary (sup.)
  #      - w inertia weight (vector of length 2 for dynamic inertia, i.e. decreasing over time)
  #      - c1 learning factor (individual experience)
  #      - c2 learning factor (social communication)
  #      - maxIter number of generations
  #      - criterion MaxDistQuick stopping criterion (with threshold)
  
  #store results
  result <- list(x.opt = numeric(d), f.opt = numeric(1), 
                 x.hist = c(), f.hist = numeric(maxIter), 
                 iter.time = c(), epoch = c(), 
                 particles = matrix(numeric(), nrow=popSize, ncol=d))
  
  #pso start time
  starttime <- Sys.time()
  
  #initialize particles
  #particle.matrix <- t(matrix((u.bound - l.bound) * runif(popSize*d) + l.bound, nrow=d, ncol=popSize))
  particle.matrix <- t(matrix(runif(popSize*d, l.bound, u.bound), nrow=d, ncol=popSize))
  
  #initial pbest 
  #f.pbest <- apply(particle.matrix, 1, f)
  pbest <- particle.matrix
  f.pbest <- apply(pbest, 1, f)
  
  #initial gbest
  gbest <- pbest[which.min(f.pbest), 1:d]
  f.gbest <- min(f.pbest)
  #f.gbest <- f(gbest)
  result$x.hist <- gbest
  result$f.hist <- f.gbest
  
  #initial velocity
  velocity <- matrix(runif(popSize*d), nrow=popSize, ncol=d)
  
  #update velocity
  velocity <- max(w) * velocity +
    c1 * runif(1) * (pbest - particle.matrix) +
    c2 * runif(1) * (t(matrix(rep(gbest, popSize), d, popSize)) - particle.matrix)
  
  #new coordinates
  particle.matrix <- particle.matrix + velocity
  
  #first iteration end
  result$iter.time <- Sys.time() - starttime 
  
  #iterate over particles
  for (i in 2:maxIter){
    
    #MaxDistQuick stopping criterion
    if (criterion[1] == TRUE){
      if (i/maxIter >  0.2){
        best.20 <- unname(tail(result$x.hist, floor(i*0.2)))
        max.euclidean.dist <- max(sqrt(rowSums(best.20 - t(matrix(gbest, d, floor(i*0.2))))^2))
          if (max.euclidean.dist < criterion[2]){
            result <- c(result, setNames((i-1), "no.ite"))
            break
          }
      }
    }
    
    #epoch
    start.iter <- Sys.time()
    
    #calculate the objective function for new coordinates
    f.particle.matrix <- apply(particle.matrix, 1, f)
    
    #update pbest
    pbest <- ifelse(matrix(rep(f.particle.matrix, d), nrow=popSize, ncol=d) < 
                      matrix(rep(f.pbest, d), nrow=popSize, ncol=d), particle.matrix, pbest)
    
    #update f.pbest
    f.pbest <- apply(pbest, 1, f)
    
    #update gbest, f.gbest
    gbest <- pbest[which.min(f.pbest), 1:d]
    f.gbest <- f(gbest)
    
    #append results
    result$x.hist <- rbind(result$x.hist, gbest)
    result$f.hist <- append(result$f.hist, f.gbest)
    
    #update velocity
    velocity <- (max(w) - (max(w) - min(w)) * i/maxIter) * velocity +
      c1 * runif(1) * (pbest - particle.matrix) +
      c2 * runif(1) * (t(matrix(rep(gbest, popSize), d, popSize)) - particle.matrix)
    
    #new coordinates
    particle.matrix <- particle.matrix + velocity
    
    #iteration time
    result$iter.time <- append(result$iter.time, Sys.time() - start.iter)
  }
  
  #solution
  result$x.opt <- gbest
  result$f.opt <- f.gbest
  
  #pso finish time
  result$epoch <- Sys.time() - starttime
  
  #end state particles' coordinates
  result$particles <- particle.matrix
  
  return(result)
}  

