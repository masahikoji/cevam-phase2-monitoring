# run_supplement_sensitivity.R
# Supplement-only sensitivity analyses for objective weighting and BOP2-FE grid refinement.
# The main manuscript DOES NOT use w. This file deliberately keeps w as sensitivity only.
source("cevam_current_core.R")
out_dir <- getOption("cevam.output_dir", "results_supplement")
if (!dir.exists(out_dir)) dir.create(out_dir,recursive=TRUE)
methods <- c("CEVAM","BOP2-FE1","BOP2-FE2")

# Anchor objective-weight sensitivity: w=1 (EN0), 1/2, 0 (EN1).
settings <- list(p0=.20,p1=.40,looks=c(10,15,20,25,30,35,40),alpha=.10,power=.88)
rows <- list(); k<-0L
for (w in c(1,.5,0)) for (m in methods) {
  k<-k+1L; d<-cevam_choose_design(settings$p0,settings$p1,settings$looks,settings$alpha,settings$power,
                                  m,criterion="weighted",weight=w)
  rows[[k]]<-data.frame(weight=w,cevam_design_to_row(d,"anchor"))
}
write.csv(do.call(rbind,rows),file.path(out_dir,"supp_objective_weight_anchor.csv"),row.names=FALSE)

# BOP2-FE grid refinement at the anchor: halve both lambda and shape spacing.
rows <- list(); k<-0L
for (m in methods) for (grid in c("base","fine")) {
  k<-k+1L
  ls <- if(grid=="base") .01 else .005; ss <- if(grid=="base") .1 else .05
  d <- cevam_choose_design(settings$p0,settings$p1,settings$looks,settings$alpha,settings$power,m,
                           criterion="EN0",lambda_step=ls,shape_step=ss)
  rows[[k]]<-data.frame(grid=grid,lambda_step=ls,shape_step=ss,cevam_design_to_row(d,"anchor"))
}
write.csv(do.call(rbind,rows),file.path(out_dir,"supp_grid_refinement_anchor.csv"),row.names=FALSE)

# Benchmark sensitivity: keep anchor-selected rules fixed and vary the true response probability.
basefits <- lapply(methods,function(m) cevam_choose_design(.20,.40,settings$looks,.10,.88,m,criterion="EN0")); names(basefits)<-methods
truths <- c(.15,.18,.20,.22,.25,.30,.40)
out <- list(); k<-0L
for (p in truths) for (m in methods) {
  k<-k+1L; z<-cevam_exact_oc(p,basefits[[m]])
  out[[k]]<-data.frame(p=p,method=m,rejection=z$rejection,EN=z$EN)
}
write.csv(do.call(rbind,out),file.path(out_dir,"supp_benchmark_sensitivity.csv"),row.names=FALSE)
message("Supplement sensitivity outputs written to ",normalizePath(out_dir,mustWork=FALSE))
