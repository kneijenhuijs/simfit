#' Run repeated measures ANOVA for two-factor within-subject design using \code{\link[ez]{ezANOVA}}
#' 
#' Runs a Repeated Measures ANOVA; NB: this function is for 
#' two-factor within-subject design with one variable representing 
#' condition and another variable representing measurement time.
#' This function was developed for use in simulations using \code{\link[simgen]{mcRun}}
#' 
#' \code{fitRManova} calculates p-values by using type III F-tests
#' 
#' @param mcr.data A dataframe formatted as described in \code{\link{generate_data}}.
#' @return A vector with elements:
#' \item{f.Cond}{F-statistic for the condition effect}
#' \item{pf.Cond}{p-value for the F-statistic of condition}
#' \item{f.Meas}{F-statistic for the measurement time effect}
#' \item{pf.Meas}{p-value for the F-statistic of measurement time}
#' \item{f.Int}{F-statistic for the interaction effect}
#' \item{pf.Int}{p-value for the F-statistic of interaction}
#' @seealso \code{\link{generate_data}} \code{\link[simgen]{mcRun}}
#' @examples
#' 
#' #Design with 4 condition levels
#' design_args <- list(ivs = c(Cond = 4), n_item = 24, n_rep=2)
#' pop_params <- gen_pop(design_args, 24)
#' xd <- sim_norm(design_args, 12, pop_params)
#' colnames(xd)[colnames(xd)=="n_rep"] <- 'Meas'
#' colnames(xd)[colnames(xd)=="Y"] <- 'Resp'
#' colnames(xd)[colnames(xd)=="subj_id"] <- 'SubjID'
#' colnames(xd)[colnames(xd)=="item_id"] <- 'ItemID'
#' xd <- with_dev_pred(xd, c("Cond", "Meas")) 
#' 
#' # run RM Anova
#' fitRManova(xd)

#' #RM anova with 4 condition levels for use in mcRun to run 1000 simulations
#' my_des <- list(ivs = c(Cond = 4), n_item = 24, n_rep = 2)
#' pops <- replicate(1000, gen_pop(my_des, 24), simplify = FALSE)
#' 
#' result <- mcRun("fitRManova",
#' mcr.datFn = "generate_data",
#' mcr.constant = list(design_args = my_des, n_subj = 24),
#' mcr.varying = pops) 
#' 
#' @export fitRManova

fitRManova <- function(mcr.data) { 
  xd <- mcr.data
  xd <- xd[complete.cases(xd),] #Delete missing cases as ezANOVA doesn't do this itself. Using RM, deleting missing cases is the only option besides imputation which we aren't using.
  
  library(ez, quietly=TRUE)
  options(warn = -1) 
  RM.all <- ezANOVA(data=xd, dv = .(Resp), wid = .(SubjID), within = .(Cond,Meas), type = 3, detailed=TRUE) #Make sure I catch the model summary here
    
  f.Cond <- RM.all$ANOVA$`F`[2]
  f.Cond.df <- RM.all$ANOVA$`DFn`[2]
  pf.Cond <- RM.all$ANOVA$`p`[2]
  f.Meas <- RM.all$ANOVA$`F`[3]
  f.Meas.df <- RM.all$ANOVA$`DFn`[3]
  pf.Meas <- RM.all$ANOVA$`p`[3]
  f.Int <- RM.all$ANOVA$`F`[4]
  f.Int.df <- RM.all$ANOVA$`DFn`[4]
  pf.Int <- RM.all$ANOVA$`p`[4]
  f.denom <- RM.all$ANOVA$`DFd`[2]
  
  v1 <- c(f.Cond=f.Cond, f.Meas=f.Meas, f.Int=f.Int,
          pf.Cond=pf.Cond, pf.Meas=pf.Meas, pf.Int=pf.Int)
  return(v1)
}