# Very Very Basic Population Sim

OlisPopSim <- function(populationsize,birthprob,deathprob){
  birthcount <- 0
  deathcount <- 0
  for(i in 1:populationsize){
    udeath <- runif(1,0,1)
    if(udeath<deathprob){deathcount <- deathcount+1}
    ubirth <- runif(1,0,1)
    if(ubirth<birthprob){birthcount <- birthcount+1}
  }
  newpopulationsize <- populationsize+birthcount-deathcount
  return(list("Pop"=newpopulationsize,"Births"=birthcount,"Deaths"=deathcount))
}

OlisPopSim(100000,0.2,0.05)
