# run_main_analysis.R
# Reproduces the CURRENT main-paper binary analysis.
# Design selection is exact; Monte Carlo is validation only.

source("cevam_current_core.R")

out_dir <- getOption("cevam.output_dir", "results_main")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive=TRUE)
nsim_validate <- as.integer(getOption("cevam.nsim", 200000L))
run_mc_all <- isTRUE(getOption("cevam.run_mc_all", FALSE))
seed0 <- as.integer(getOption("cevam.seed", 71926L))
methods <- c("CEVAM","BOP2-FE1","BOP2-FE2")

fit_case <- function(label,p0,p1,N,looks,alpha,power) {
  fits <- lapply(methods, function(m) cevam_choose_design(p0,p1,looks,alpha,power,m,criterion="EN0"))
  names(fits) <- methods
  if (any(vapply(fits,is.null,logical(1)))) stop("infeasible method in ",label)
  rows <- do.call(rbind,lapply(fits,cevam_design_to_row,scenario=label))
  rows$p0 <- p0; rows$p1 <- p1; rows$N <- N; rows$looks <- paste(looks,collapse=",")
  rows$alpha <- alpha; rows$target_power <- power
  list(fits=fits,rows=rows)
}

# Anchor configuration
anchor <- fit_case("anchor",.20,.40,40L,c(10,15,20,25,30,35,40),.10,.88)
write.csv(anchor$rows,file.path(out_dir,"main_anchor_exact.csv"),row.names=FALSE)

# Boundary table
bnd <- do.call(rbind,lapply(anchor$fits,function(d) {
  data.frame(method=d$method,look=c(d$looks[-length(d$looks)],tail(d$looks,1)),
             lower=c(d$lower,NA),upper=c(d$upper,d$terminal),
             final=c(rep(FALSE,length(d$looks)-1),TRUE))
}))
write.csv(bnd,file.path(out_dir,"main_anchor_boundaries.csv"),row.names=FALSE)

# Monte Carlo validation for anchor (both p0 and p1)
mc_result_row <- function(method, truth, z, scenario = NULL) {
  vals <- as.list(z)
  out <- data.frame(
    method = as.character(method),
    truth = as.numeric(truth),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (nm in names(vals)) out[[nm]] <- vals[[nm]]
  if (!is.null(scenario)) out <- data.frame(scenario = as.character(scenario), out, stringsAsFactors = FALSE, check.names = FALSE)
  out
}

mc_anchor <- do.call(rbind,lapply(seq_along(anchor$fits),function(j) {
  d <- anchor$fits[[j]]
  a <- cevam_mc_binary(.20,d,nsim_validate,seed0+10*j)
  b <- cevam_mc_binary(.40,d,nsim_validate,seed0+10*j+1L)
  rbind(
    mc_result_row(d$method, .20, a),
    mc_result_row(d$method, .40, b)
  )
}))
write.csv(mc_anchor,file.path(out_dir,"main_anchor_mc_validation.csv"),row.names=FALSE)

# 34 configurations used in the manuscript.
scenario_rows <- list(); scenario_fits <- list(); sid <- 0L
for (pair in list(c(.10,.25),c(.20,.40),c(.40,.60),c(.60,.75))) {
  p0 <- pair[1]; p1 <- pair[2]
  for (alpha in c(.05,.10)) for (power in c(.80,.90)) {
    N <- cevam_fixed_reference_N(p0,p1,alpha,power+.03)
    for (frequency in c(2L,7L)) {
      looks <- if (frequency==2L) c(as.integer(5*round(.5*N/5)),N) else
        sort(unique(as.integer(5*round(c(.25,.375,.5,.625,.75,.875,1)*N/5))))
      sid <- sid+1L; label <- sprintf("%02d",sid)
      z <- fit_case(label,p0,p1,N,looks,alpha,power)
      scenario_rows[[sid]] <- z$rows; scenario_fits[[sid]] <- z$fits
      message("Completed scenario ",label)
    }
  }
}
for (frequency in c(2L,7L)) {
  p0 <- .20; p1 <- .30; alpha <- .05; power <- .80
  N <- cevam_fixed_reference_N(p0,p1,alpha,power+.03)
  looks <- if (frequency==2L) c(as.integer(N/2),N) else
    sort(unique(as.integer(5*round(c(.25,.375,.5,.625,.75,.875,1)*N/5))))
  sid <- sid+1L; label <- sprintf("%02d",sid)
  z <- fit_case(label,p0,p1,N,looks,alpha,power)
  scenario_rows[[sid]] <- z$rows; scenario_fits[[sid]] <- z$fits
  message("Completed scenario ",label)
}
allres <- do.call(rbind,scenario_rows)
write.csv(allres,file.path(out_dir,"main_34config_exact.csv"),row.names=FALSE)

# Summary of EN0 differences. Negative percentages favor CEVAM.
wide <- split(allres,allres$scenario)
summ <- do.call(rbind,lapply(c("BOP2-FE1","BOP2-FE2"),function(comp) {
  dif <- sapply(wide,function(x) 100*(x$EN0[x$method=="CEVAM"]/x$EN0[x$method==comp]-1))
  data.frame(comparator=comp,minimum=min(dif),median=median(dif),maximum=max(dif),
             lower=sum(dif < -1e-10),equal=sum(abs(dif)<=1e-10),higher=sum(dif>1e-10))
}))
write.csv(summ,file.path(out_dir,"main_34config_EN0_summary.csv"),row.names=FALSE)
print(summ)

# Optional MC validation for every selected design at p0 and p1.
if (run_mc_all) {
  mc <- list(); k <- 0L
  for (i in seq_along(scenario_fits)) {
    dat <- scenario_rows[[i]][1,]
    for (j in seq_along(scenario_fits[[i]])) {
      d <- scenario_fits[[i]][[j]]
      for (truth in c(dat$p0,dat$p1)) {
        k<-k+1L; z<-cevam_mc_binary(truth,d,nsim_validate,seed0+1000*i+10*j+as.integer(100*truth))
        mc[[k]] <- mc_result_row(d$method, truth, z, scenario = sprintf("%02d", i))
      }
    }
  }
  write.csv(do.call(rbind,mc),file.path(out_dir,"main_34config_mc_validation.csv"),row.names=FALSE)
}

# Machine-readable R objects for later table generation.
saveRDS(list(anchor=anchor,scenario_rows=scenario_rows,scenario_fits=scenario_fits,summary=summ),
        file.path(out_dir,"main_analysis_objects.rds"))
message("Main analysis written to: ",normalizePath(out_dir,mustWork=FALSE))
