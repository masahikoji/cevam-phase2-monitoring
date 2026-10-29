# run_supplement_joint_mc.R
# Supplement joint-information sensitivity study.
# Monte Carlo only; this is not the primary design-selection analysis.
source("cevam_current_core.R")
out_dir<-getOption("cevam.output_dir","results_supplement")
if(!dir.exists(out_dir)) dir.create(out_dir,recursive=TRUE)
nsim<-as.integer(getOption("cevam.joint_nsim",100000L));seed0<-as.integer(getOption("cevam.seed",71926L))
N<-140L;looks<-c(35L,70L,105L,140L);phi0<-c(.45,.70);phi1<-c(.60,.80);alpha<-.10;beta<-.20;Q<-c(.08,.32,.12,.48)

safe_xlogx <- function(x,y) ifelse(x==0,0,x*log(y))
run_one <- function(truth,seed) {
  set.seed(seed); counts<-matrix(0L,nsim,4);status<-matrix(0L,nsim,2);stopn<-matrix(N,nsim,2);prev<-0L
  for(n in looks) {
    add<-t(rmultinom(nsim,n-prev,truth));counts<-counts+add;prev<-n
    s<-cbind(counts[,3]+counts[,4],counts[,2]+counts[,4])
    lrm<-sweep(s,2,log(phi1/phi0),"*")+sweep(n-s,2,log((1-phi1)/(1-phi0)),"*")
    marginal_success<-lrm[,1]>=log(1/alpha) & lrm[,2]>=log(1/alpha)
    fut<-lrm[,1]<=log(beta/2) | lrm[,2]<=log(beta/2)
    sat<-rowSums(ifelse(counts==0,0,counts*log(counts/n)))
    llfaces<-matrix(NA_real_,nsim,2)
    for(j in 1:2) {
      sj<-s[,j]
      term<-rowSums(ifelse(counts==0,0,counts*log(pmax(counts,1)))) - ifelse(sj==0,0,sj*log(sj)) - ifelse(n-sj==0,0,(n-sj)*log(n-sj)) + sj*log(phi0[j])+(n-sj)*log(1-phi0[j])
      llfaces[,j]<-ifelse(sj/n<=phi0[j],sat,term)
    }
    ll0<-pmax(llfaces[,1],llfaces[,2]); logE<-as.vector(counts %*% log(Q))-ll0
    if(any(logE > pmin(lrm[,1],lrm[,2])+1e-8)) stop("pathwise profile <= marginal LR check failed")
    joint_success<-logE>=log(1/alpha)
    for(j in 1:2) {
      suc<-if(j==1) marginal_success else joint_success
      active<-status[,j]==0L;win<-active&suc;lose<-active&!suc&fut
      status[win,j]<-1L;status[lose,j]<--1L;stopn[win|lose,j]<-n
    }
  }
  if(any(status[,2]==1L & status[,1]!=1L)) stop("pathwise rejection ordering failed")
  if(any(stopn[,1]>stopn[,2])) stop("pathwise stopping ordering failed")
  ans<-list()
  for(j in 1:2) {
    r<-mean(status[,j]==1L);en<-mean(stopn[,j]);ans[[j]]<-c(rejection=r,MCSE_rejection=sqrt(r*(1-r)/nsim),EN=en,MCSE_EN=sd(stopn[,j])/sqrt(nsim))
  }
  names(ans)<-c("Marginal","Profile_joint");ans
}

cases<-list(both_null=c(.45,.70),efficacy_null=c(.45,.95),safety_null=c(.80,.70),alternative=c(.60,.80));rows<-list();k<-0L
for(label in names(cases)) {
  p<-cases[[label]];assocs<-list(Negative=max(0,sum(p)-1),Independent=prod(p),Positive=min(p))
  for(ass in names(assocs)) {
    q11<-assocs[[ass]];truth<-c(1-sum(p)+q11,p[2]-q11,p[1]-q11,q11);truth<-pmax(truth,0);truth<-truth/sum(truth)
    z<-run_one(truth,seed0+k);k<-k+1L
    for(rule in names(z)) rows[[length(rows)+1L]]<-data.frame(case=label,association=ass,rule=rule,t(z[[rule]]))
  }
}
write.csv(do.call(rbind,rows),file.path(out_dir,"supp_joint_profile_mc.csv"),row.names=FALSE)
