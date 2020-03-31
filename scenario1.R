n.simulation.runs <- 1e3
n.people <- 1500 # persons in simulation
n.days <- 365 # number of days that will be simulated
n.buildings <- 3 # buildings in the area, people are split between these
outside.connection <- c(20, 40) # these many people enter and leave every day
detection.probability <- .8 # if one has corona, how likely to be detected and subsequently go home?
risky.time.building <- 1/20 # time (in hours) that people risk interacting with others in their building while arriving, going for lunch, or leaving
base.probability.contraction <- .9 # per contact, how likely is an infected person to infect another?
social.distancing.proportion <- .25 # which percentage of people try to practice social distancing?
social.distancing.effectiveness <- .8 # in each interaction, how likely is social distancing to work?
arrival.range <- c(7, 8) # time that people arrive in the morning (in hours eg 12 is noon)
lunch.range <- c(12, 13.5) # time that people leave the building for lunch (in hours eg 12 is noon)
leave.range <- c(17, 18.5) # time that people leave the building (in hours eg 12 is noon)

# setup data frame that contains person data
people <- data.frame(
  age = runif(n.people, 19, 65),
  behaviour = sample(c("nolunch", "lunch"), n.people, T, c(2, 1)),
  corona.status = sample(c(FALSE, TRUE), n.people, T, c(9, 1)),
  detected = FALSE,
  social.distancing = sample(c(FALSE, TRUE), n.people, T, c(1 - social.distancing.proportion, social.distancing.proportion)),
  works.in.building = sample(1:n.buildings, n.people, T),
  arrival.time = NA,
  lunch.time = NA,
  leave.time = NA
)

# what we will collect for plotting at the end
prevalence.overall <- prevalence <- matrix(0, nrow = n.buildings, ncol = n.days)

detected.overall <- rep(0, n.days)
ICU.load <- rep(0, n.days)

for(run in 1:n.simulation.runs){
  for(day in 1:n.days){
    message("Day ", day, ".\nThere are ", sum(people$corona.status), " infected people.")
    # people leave (only if enough people still here)
    if(nrow(people) > outside.connection[1]){
      n.leaving <- runif(1, outside.connection[1], outside.connection[2])
      people <- people[-(sample(nrow(people), n.leaving, T)), ] # between 200 and 400 people leave 
    }
    
    # people go to work. uniform distribution assumed for times
    people$arrival.time = runif(nrow(people), arrival.range[1], arrival.range[2])
    people$lunch.time = runif(nrow(people), lunch.range[1], lunch.range[2])
    people$leave.time = runif(nrow(people), leave.range[1], leave.range[2])
    
    # people are detected
    detected <- which(people$corona.status == TRUE & runif(nrow(people)) < detection.probability)
    if(length(detected) > 0){
      detected.overall[day] <- detected.overall[day] + length(detected)
      message(length(detected)," patients detected and quarantined.")
      people <- people[-detected, ]
    }
    
    # infections happen
    if(sum(people$corona.status & !people$detected) > 0){
      for(p in which(people$corona.status & !people$detected)){
        contacts <- NULL
        # which people are in my building and arrive or leave within risky period?
        contacts.office <- which(
          people$works.in.building == people$works.in.building[p] & # need to be in my building
            # !people$corona.status & # not already infected
            (abs(people$arrival.time - people$arrival.time[p]) <= risky.time.building | # either arrive close to me ...
               abs(people$leave.time - people$leave.time[p]) <= risky.time.building) # or leave close to me
        )
        contacts <- contacts.office
        # which people go for lunch in my building within the risky period? only relevant if I am a lunch goer
        if(people$behaviour[p] == "lunch"){
          contacts.lunch <- which(
            people$works.in.building == people$works.in.building[p] & # need to be in my building
              #!people$corona.status & # not already infected
              people$behaviour == "lunch" & # need to also go to lunch
              abs(people$lunch.time - people$lunch.time[p]) <= risky.time.building # have lunch close to my time (within risky period)
          )
          contacts <- c(contacts.office, contacts.lunch)
        }
        # all contacts have a risk of contracting corona, conditional on infected person's social distancing behaviour
        if(length(contacts) > 0){
          for(contact in contacts){
            if(people$social.distancing[p] & runif(1) > social.distancing.effectiveness){
              people$corona.status[contact] <- TRUE
            } else if(runif(1) < base.probability.contraction){
              people$corona.status[contact] <- TRUE
            }
          }
        }
      }
    }
    
    # people enter. every day, more corona arrives
    n.entering <- runif(1, outside.connection[1], outside.connection[2])
    new.people <- data.frame(
      age = runif(n.entering, 19, 65),
      behaviour = sample(c("nolunch", "lunch"), n.entering, T, c(2, 1)),
      corona.status = FALSE,
      detected = FALSE,
      social.distancing = sample(c(FALSE, TRUE), n.entering, T, c(1 - social.distancing.proportion, social.distancing.proportion)),
      works.in.building = sample(1:n.buildings, n.entering, T),
      arrival.time = NA,
      lunch.time = NA,
      leave.time = NA
    )
    cases.in.enterings <- min(round(day / 10), nrow(new.people))
    new.people$corona.status[1:cases.in.enterings] <- TRUE
    people <- rbind(
      people,
      new.people
    )
    
    # calculate metrics
    for(b in 1:n.buildings){
      prevalence[b, day] <- mean(people$corona.status[people$works.in.building == b])
    }
  }
  

  
  prevalence.overall <- prevalence.overall + prevalence
}

prevalence.overall <- prevalence.overall / n.simulation.runs


par(mfcol=c(1, 2))
plot(prevalence.overall[1, ],tlab="% infected", xlab="day", type="l", col=rainbow(n.buildings)[1], ylim=c(0, max(prevalence.overall)), lwd=3,main="Prevalence per building")
for(i in 2:n.buildings){
  lines(prevalence.overall[i, ], col=rainbow(n.buildings)[i], lwd=3)
}
plot(cumsum(detected.overall/n.simulation.runs), type = "l", main="Cumulative detected cases", lwd=3, xlab="day", ylab="cases")
par(mfcol=c(1, 1))