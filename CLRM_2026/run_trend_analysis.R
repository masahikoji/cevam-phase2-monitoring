# run_trend_analysis.R
# Current TREND feasibility calculation and prospective CEVAM redesign.
source("cevam_current_core.R")
out_dir <- getOption("cevam.output_dir", "results_trend")
if (!dir.exists(out_dir)) dir.create(out_dir,recursive=TRUE)

p0 <- .41; p1 <- .56; alpha <- .05; target_power <- .80
ceil65 <- cevam_np_ceiling(p0,p1,65L,alpha)
firstN <- NA_integer_
for (N in 1:200) if (cevam_np_ceiling(p0,p1,N,alpha)$power >= target_power) { firstN<-N; break }

# Manuscript illustration: reviews at 48 and 75.
fits <- lapply(c("CEVAM","BOP2-FE1","BOP2-FE2"),function(m)
  cevam_choose_design(p0,p1,c(48L,75L),alpha,target_power,m,criterion="EN0"))
names(fits)<-c("CEVAM","BOP2-FE1","BOP2-FE2")
rows <- do.call(rbind,lapply(fits,function(d) if (is.null(d)) NULL else cevam_design_to_row(d,"TREND_N75")))
write.csv(rows,file.path(out_dir,"trend_N75_exact.csv"),row.names=FALSE)
write.csv(data.frame(N=65,ceiling_power=ceil65$power,randomize_at=ceil65$randomize_at,
                     randomize_probability=ceil65$randomize_probability,first_N_reaching_80pct=firstN),
          file.path(out_dir,"trend_feasibility.csv"),row.names=FALSE)
saveRDS(list(ceiling65=ceil65,firstN=firstN,fits=fits),file.path(out_dir,"trend_objects.rds"))
print(rows)
