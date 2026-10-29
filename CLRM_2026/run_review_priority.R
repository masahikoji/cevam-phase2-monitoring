# Statistics in Medicine manuscript 1559798: prioritized reviewer calculations.
# Uses the exact current CEVAM binary engine without modifying it.
# Usage (Mac/Linux, in a folder with cevam_current_core.R):
#   Rscript run_review_priority.R --mode=baseline --cores=1
#   Rscript run_review_priority.R --mode=prior --cores=12
#   Rscript run_review_priority.R --mode=all --cores=12
#   Rscript run_review_priority.R --mode=prior --cores=1 --subset=anchor
# Outputs are checkpointed to review_results/; reruns skip completed cases.

args <- commandArgs(trailingOnly=TRUE)
get_arg <- function(key, default) {
  z <- grep(paste0('^--', key, '='), args, value=TRUE)
  if (length(z)==0L) return(default)
  sub(paste0('^--',key,'='), '', z[[length(z)]])
}
mode <- get_arg('mode','all')
if (!(mode %in% c('all','baseline','prior'))) stop('mode must be all, baseline, or prior')
subset <- get_arg('subset','all')
if (!(subset %in% c('all','anchor'))) stop('subset must be all or anchor')
cores <- as.integer(get_arg('cores','1'))
if (is.na(cores) || cores < 1L) stop('cores must be a positive integer')
base <- getwd()
core_name <- if (file.exists(file.path(base,'cevam_current_core.R'))) {
  'cevam_current_core.R'
} else if (file.exists(file.path(base,'cevam_current_core(1).R'))) {
  'cevam_current_core(1).R'
} else stop('Put the current core R file in the working directory')
source(file.path(base,core_name))
result_dir <- file.path(base,'review_results')
dir.create(result_dir,recursive=TRUE,showWarnings=FALSE)

cases <- list(list(id='anchor',p0=.20,p1=.40,N=40L,
                   looks=c(10L,15L,20L,25L,30L,35L,40L),alpha=.10,power=.88))
id <- 0L
for (pair in list(c(.10,.25),c(.20,.40),c(.40,.60),c(.60,.75))) {
  p0 <- pair[1L]; p1 <- pair[2L]
  for (alpha in c(.05,.10)) for (pw in c(.80,.90)) {
    N <- cevam_fixed_reference_N(p0,p1,alpha,pw+.03)
    for (freq in c(2L,7L)) {
      looks <- if (freq==2L) {
        c(as.integer(5*round(.5*N/5)),as.integer(N))
      } else {
        sort(unique(as.integer(5*round(c(.25,.375,.5,.625,.75,.875,1)*N/5))))
      }
      id <- id+1L
      cases[[length(cases)+1L]] <- list(id=sprintf('%02d',id),p0=p0,p1=p1,
                                       N=as.integer(N),looks=looks,
                                       alpha=alpha,power=pw)
    }
  }
}
for (freq in c(2L,7L)) {
  p0 <- .20; p1 <- .30; alpha <- .05; pw <- .80
  N <- cevam_fixed_reference_N(p0,p1,alpha,pw+.03)
  looks <- if (freq==2L) c(as.integer(N/2),as.integer(N)) else
    sort(unique(as.integer(5*round(c(.25,.375,.5,.625,.75,.875,1)*N/5))))
  id <- id+1L
  cases[[length(cases)+1L]] <- list(id=sprintf('%02d',id),p0=p0,p1=p1,
                                   N=as.integer(N),looks=looks,alpha=alpha,power=pw)
}
stopifnot(id==34L,length(cases)==35L)
if (subset=='anchor') cases <- cases[1L]

atomic_csv <- function(d, filename) {
  tmp <- tempfile('review_checkpoint_',tmpdir=dirname(filename),fileext='.csv')
  write.csv(d,tmp,row.names=FALSE)
  if (!file.rename(tmp,filename)) stop('Could not move temporary result to ',filename)
}

# Fixed-threshold e-process reference, no binding futility.
# This is the common fixed E >= 1/alpha rejection rule at every planned look.
run_baseline <- function(s) {
  a <- log(s$p1*(1-s$p0)/(s$p0*(1-s$p1)))
  b0 <- log((1-s$p1)/(1-s$p0)) # NEGATIVE, as in the current core.
  u <- as.integer(ceiling((log(1/s$alpha)-b0*s$looks)/a - 1e-12))
  k <- length(s$looks)
  d <- list(looks=as.integer(s$looks),lower=rep(-1L,k-1L),
            upper=u[-k],terminal=u[k])
  z0 <- cevam_exact_oc(s$p0,d)
  z1 <- cevam_exact_oc(s$p1,d)
  data.frame(scenario=s$id,method='Fixed-LR-e-process',prior='None',
             p0=s$p0,p1=s$p1,alpha=s$alpha,target_power=s$power,N=s$N,
             looks=paste(s$looks,collapse=','),
             upper=paste(u,collapse=','),size=z0$rejection,power=z1$rejection,
             EN0=z0$EN,EN1=z1$EN,meets_power=z1$rejection>=s$power-1e-12)
}

# For each Beta prior, RE-OPTIMIZE both BOP2-FE families under exactly the
# same size, power, and null expected sample size objective.
run_priors <- function(s) {
  rows <- list()
  for (label in c('Jeffreys','Uniform')) {
    prior <- if(label=='Jeffreys') c(.5,.5) else c(1,1)
    for (method in c('BOP2-FE1','BOP2-FE2')) {
      d <- cevam_choose_design(s$p0,s$p1,s$looks,s$alpha,s$power,
                               method=method,criterion='EN0',prior=prior)
      n <- length(rows)+1L
      if (is.null(d)) {
        rows[[n]] <- data.frame(scenario=s$id,prior=label,method=method,
                                p0=s$p0,p1=s$p1,alpha=s$alpha,target_power=s$power,
                                N=s$N,feasible=FALSE,size=NA_real_,power=NA_real_,
                                EN0=NA_real_,EN1=NA_real_,lower=NA_character_,
                                upper=NA_character_,terminal=NA_integer_)
      } else {
        rows[[n]] <- data.frame(scenario=s$id,prior=label,method=method,
                                p0=s$p0,p1=s$p1,alpha=s$alpha,target_power=s$power,
                                N=s$N,feasible=TRUE,size=d$size,power=d$power,
                                EN0=d$EN0,EN1=d$EN1,
                                lower=paste(d$lower,collapse=','),
                                upper=paste(d$upper,collapse=','),terminal=d$terminal)
      }
    }
  }
  do.call(rbind,rows)
}

run_jobs <- function(label,fun) {
  jobs <- lapply(cases,function(s) {
    dest <- file.path(result_dir,paste0('case_',s$id,'_',label,'.csv'))
    list(s=s,path=dest)
  })
  pending <- jobs[!vapply(jobs,function(j)file.exists(j$path),logical(1))]
  message(label,': ',length(jobs)-length(pending),' already completed; ',
          length(pending),' remain')
  if (length(pending)>0L) {
    worker <- function(j) {
      tryCatch({
        z <- fun(j$s)
        atomic_csv(z,j$path)
        paste('OK',j$s$id)
      },error=function(e) paste('ERROR',j$s$id,conditionMessage(e)))
    }
    msgs <- if (cores>1L && .Platform$OS.type=='unix') {
      unlist(parallel::mclapply(pending,worker,mc.cores=cores),use.names=FALSE)
    } else unlist(lapply(pending,worker),use.names=FALSE)
    cat(paste(msgs,collapse='\n'),'\n')
    if (any(grepl('^ERROR',msgs))) stop(label,': some jobs failed. Rerun after fixing the problem.')
  }
  paths <- vapply(jobs,function(j)j$path,character(1))
  if (!all(file.exists(paths))) stop('Missing checkpoint files for ',label)
  allres <- do.call(rbind,lapply(paths,read.csv,stringsAsFactors=FALSE,
                                colClasses=c(scenario='character')))
  out <- file.path(result_dir,paste0('review_',label,'_all.csv'))
  atomic_csv(allres,out)
  message('Wrote ',out,' (',nrow(allres),' rows)')
  if (label=='fixed_e') {
    message('Fixed-threshold rules achieving target power: ',sum(allres$meets_power),
            ' / ',nrow(allres))
  }
  invisible(allres)
}

if (mode %in% c('all','baseline')) run_jobs('fixed_e',run_baseline)
if (mode %in% c('all','prior')) run_jobs('priors',run_priors)
message('Review calculations finished. R and source-file versions should be archived with the outputs.')
