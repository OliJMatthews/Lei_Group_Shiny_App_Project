library(IBMPopSim)
init_size <- 10000000
pop_init <- population(data.frame(birth = rep(0, init_size), death = NA))
birth = mk_event_poisson(type = 'birth', intensity = 'lambda')
death = mk_event_poisson(type = 'death', intensity = 'mu')
params = list('lambda' = 100000000, 'mu' = 1000000)
# mk_model compiles C++ code using sourceCpp from Rcpp
birth_death <- mk_model(characteristics = get_characteristics(pop_init),
                        events = list(birth, death),
                        parameters = params)
sim_out <- popsim(model = birth_death,
                  initial_population = pop_init,
                  events_bounds = c('birth' = params$lambda, 'death' = params$mu),
                  parameters = params,
                  time = 10)
num_births <- length(sim_out$population$birth) - init_size
num_deaths <- sum(!is.na(sim_out$population$death))
print(c("births" = num_births, "deaths" = num_deaths))
