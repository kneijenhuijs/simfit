#' Generates two-factor within-subject design using (\code{\link[funfact]{sim_norm}})
#' 
#' Generates two-factor within-subject design with one variable representing condition 
#' and another variable representing measurement time. This function is designed for
#' generating data for simulations run by \code{\link[simgen]{mcRun}}.
#' 
#' @param mcr.params Design arguments  as used by \code{\link[funfact]{sim_norm}}. For details
#' see \code{\link[funfact]{stim_lists}}.
#' 
#' @seealso \code{\link[funfact]{sim_norm}}, \code{\link[simgen]{mcRun}}, \code{\link{fitlmer.facWithinABMax}}, \code{\link{fitlmer.facWithinABNoCovar}}
#' @examples
#' 
#' #Simulation fitting two-factor maximal mixed-effect models
#' my_des <- list(ivs = c(Cond = 4), n_item = 24, n_rep = 2)
#' pops <- replicate(1000, gen_pop(my_des, 24), simplify = FALSE)
#' 
#' result <- mcRun("fitlmer.facWithinABMax",
#' mcr.fnArgs = list(ncond=4)
#' mcr.datFn = "generate_data",
#' mcr.constant = list(design_args = my_des, n_subj = 24),
#' mcr.varying = pops) 
#' 
#' @export generate_data

generate_data <- function(mcr.params) {
  library(funfact, quietly=TRUE)
  xd <- sim_norm(mcr.params[["design_args"]],
                 mcr.params[["n_subj"]],
                 mcr.params[setdiff(names(mcr.params), c("design_args", "n_subj"))])
  colnames(xd)[colnames(xd)=="n_rep"] <- 'Meas'
  colnames(xd)[colnames(xd)=="Y"] <- 'Resp'
  colnames(xd)[colnames(xd)=="subj_id"] <- 'SubjID'
  colnames(xd)[colnames(xd)=="item_id"] <- 'ItemID'
  xd$SubjID <- as.factor(xd$SubjID)
  xd$Cond <- as.factor(xd$Cond)
  xd$Meas <- as.factor(xd$Meas)
  xd <- with_dev_pred(xd, c("Cond", "Meas"))
}