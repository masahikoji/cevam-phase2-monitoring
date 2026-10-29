# run_supplement_complex.R
# Current supplement: dependence-robust component certificates and exact joint OCs.
source("cevam_current_core.R")
out_dir <- getOption("cevam.output_dir","results_supplement")
if (!dir.exists(out_dir)) dir.create(out_dir,recursive=TRUE)
methods <- c("CEVAM","BOP2-FE1","BOP2-FE2")

certified_component <- function(p0,p1,looks,alpha_budget,beta_budget,method) {
  # Candidate family as in main design. Positive envelope ignores all futility;
  # negative envelope ignores all efficacy. Main selection minimizes actual EN0.
  cand <- if (method=="CEVAM") cevam_lr_boundary_tables(p0,p1,looks) else bop2fe_candidates(p0,looks,method)
  best <- NULL; best_key <- NULL
  for (i in seq_along(cand)) {
    z <- cand[[i]]
    # Positive envelope: no interim futility.
    never_lower <- rep(-1L,length(looks)-1L)
    pos <- if(method=="CEVAM") .cevam_eval_free_terminal(p0,p1,looks,never_lower,z$upper,alpha_budget-1e-12) else
      .cevam_eval_fixed_terminal(p0,p1,looks,never_lower,z$upper,z$terminal)
    if(is.null(pos) || pos$size > alpha_budget-1e-12) next
    terminal <- pos$terminal
    # Negative envelope: no interim efficacy; non-success probability at p1 <= beta.
    never_upper <- looks[-length(looks)] + 1L
    neg <- .cevam_eval_fixed_terminal(p0,p1,looks,z$lower,never_upper,terminal)
    neg_prob <- 1-neg$power
    if(neg_prob > beta_budget-1e-12) next
    actual <- .cevam_eval_fixed_terminal(p0,p1,looks,z$lower,z$upper,terminal)
    key <- c(actual$EN0,actual$size,-actual$power,i)
    if(.cevam_lex_less(key,best_key)) {
      best_key<-key
      best<-list(method=method,looks=looks,lower=as.integer(z$lower),upper=as.integer(z$upper),terminal=terminal,
                 size=actual$size,power=actual$power,EN0=actual$EN0,EN1=actual$EN1,
                 positive_envelope=pos$size,negative_envelope=neg_prob,
                 alpha_budget=alpha_budget,beta_budget=beta_budget,parameters=z$pars %||% list())
    }
  }
  best
}

joint_cell_probs <- function(p1,p2,q11) {
  q<-c(1-p1-p2+q11,p2-q11,p1-q11,q11)
  if(any(q < -1e-12)) stop("invalid joint distribution")
  q<-pmax(q,0);q/sum(q)
}

joint_exact_oc <- function(margins,q11,d1,d2,mode=c("OR","AND")) {
  mode<-match.arg(mode); looks<-d1$looks; stopifnot(identical(looks,d2$looks));N<-tail(looks,1L)
  q<-joint_cell_probs(margins[1],margins[2],q11)
  alive<-matrix(0,N+1,N+1);alive[1,1]<-1
  reject<-fut<-early_success<-en<-0;look_index<-1L
  for(n in 1:N) {
    en<-en+sum(alive)
    v<-matrix(0,N+1,N+1)
    # vectorized four-cell shift update for patient n
    a<-alive[1:n,1:n,drop=FALSE]
    v[1:n,1:n] <- v[1:n,1:n] + q[1]*a
    v[1:n,2:(n+1)] <- v[1:n,2:(n+1)] + q[2]*a
    v[2:(n+1),1:n] <- v[2:(n+1),1:n] + q[3]*a
    v[2:(n+1),2:(n+1)] <- v[2:(n+1),2:(n+1)] + q[4]*a
    if(n==looks[look_index]) {
      for(x in 0:n) for(y in 0:n) {
        px<-v[x+1,y+1]; if(px==0) next
        if(n==N) {
          suc<-if(mode=="OR") (x>=d1$terminal || y>=d2$terminal) else (x>=d1$terminal && y>=d2$terminal)
          if(suc) reject<-reject+px
          v[x+1,y+1]<-0
        } else {
          suc<-if(mode=="OR") (x>=d1$upper[look_index] || y>=d2$upper[look_index]) else (x>=d1$upper[look_index] && y>=d2$upper[look_index])
          fail<-if(mode=="OR") (x<=d1$lower[look_index] && y<=d2$lower[look_index]) else (x<=d1$lower[look_index] || y<=d2$lower[look_index])
          if(suc) {reject<-reject+px;early_success<-early_success+px;v[x+1,y+1]<-0}
          else if(fail) {fut<-fut+px;v[x+1,y+1]<-0}
        }
      }
      look_index<-look_index+1L
    }
    alive<-v
  }
  list(rejection=reject,EN=en,early_futility=fut,early_success=early_success)
}

run_setting <- function(name,p0,p1,mode,nested=FALSE) {
  alpha<-.10;beta<-.20;a<-if(mode=="OR") alpha/2 else alpha;b<-if(mode=="OR") beta else beta/2
  found<-FALSE
  for(N in seq(40L,250L,by=5L)) {
    looks<-as.integer(5*round(c(.25,.5,.75,1)*N/5))
    dd<-lapply(methods,function(m) lapply(seq_along(p0),function(j) certified_component(p0[j],p1[j],looks,a,b,m)))
    names(dd)<-methods
    if(all(vapply(unlist(dd,recursive=FALSE),function(x)!is.null(x),logical(1)))) {found<-TRUE;break}
  }
  if(!found) stop("No common certified N for ",name)
  certrows<-list();k<-0L
  for(m in methods) for(j in 1:2) {k<-k+1L;d<-dd[[m]][[j]];certrows[[k]]<-data.frame(setting=name,method=m,component=j,N=N,
    alpha_budget=a,actual_positive=d$positive_envelope,beta_budget=b,actual_negative=d$negative_envelope,EN0=d$EN0)}
  write.csv(do.call(rbind,certrows),file.path(out_dir,paste0("supp_complex_certificates_",name,".csv")),row.names=FALSE)

  truths<-if(mode=="AND") list(all_null=p0,only_first_null=c(p0[1],min(.99,p1[2]+.15)),only_second_null=c(min(.99,p1[1]+.2),p0[2]),alternative=p1) else
    list(all_null=p0,first_promising=c(p1[1],p0[2]),second_promising=c(p0[1],p1[2]),both_promising=p1)
  rows<-list();k<-0L
  for(label in names(truths)) {
    marg<-truths[[label]]
    assocs<-if(nested) list(nested=marg[1]) else list(Frechet_lower=max(0,sum(marg)-1),independent=prod(marg),Frechet_upper=min(marg))
    for(ass in names(assocs)) for(m in methods) {
      k<-k+1L;z<-joint_exact_oc(marg,assocs[[ass]],dd[[m]][[1]],dd[[m]][[2]],mode)
      rows[[k]]<-data.frame(setting=name,truth=label,association=ass,method=m,m1=marg[1],m2=marg[2],rejection=z$rejection,EN=z$EN,
                           early_futility=z$early_futility,early_success=z$early_success)
    }
  }
  write.csv(do.call(rbind,rows),file.path(out_dir,paste0("supp_complex_joint_",name,".csv")),row.names=FALSE)
  saveRDS(list(name=name,N=N,looks=looks,designs=dd,results=rows),file.path(out_dir,paste0("supp_complex_",name,".rds")))
  message(name,": N=",N,"; looks=",paste(looks,collapse=","))
}

run_setting("nested",c(.15,.30),c(.25,.50),"OR",TRUE)
run_setting("multiple_efficacy",c(.10,.20),c(.30,.35),"OR",FALSE)
run_setting("efficacy_safety",c(.45,.70),c(.60,.80),"AND",FALSE)
