#### calculate cGDD before final freeze for a given location/year
### INPUT: DF with day, tmin, tmax:  vectors of equal legnth with reapeadted ID, min and max temp for each day for period of time in question. If  meanT is set to TRUE, day of final freeze is considered the last day where mean temp <0 rather than min temp <0
## OUTPUT: DF with Julian day of last freeze and cgdd before that day 
# BWW

freeze_cGDD <- function(x, meanT=FALSE) {

require(dplyr)
  
  
if (meanT==TRUE) {
    
    # need to calculate mean temp for each day in the given data
    meanTemps <- x %>%
       arrange(doy) %>%
       mutate(tavg=(tmin+tmax)/2)
      
      # fday is the last day where mean temp <0  
    
    # for some locations, there are no days where mean temp is <0, so I return 0
       fday <-ifelse(min(meanTemps$tavg)<0,
                     max(meanTemps$doy[which(meanTemps$tavg<0)]),
                     0)
      
   }else{
     #if we're interested in min temp not mean, no need to calculate mean
     fday  <- max(x$doy[which(x$tmin<0)])
   }
  
# find cGDD before final freeze
 x %>%
  filter(doy<fday) %>%
   mutate(GD=ifelse((tmax+tmin)/2>0,(tmax+tmin)/2,0)) %>%
   summarize(cGDD_ff= sum(GD), final_freeze=fday) 
  
}