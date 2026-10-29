# run_supplement_adaptation.R
# Exact conditional-error / conditional-power operational extension for current anchor CEVAM.
source("cevam_current_core.R")
out_dir<-getOption("cevam.output_dir","results_supplement");if(!dir.exists(out_dir)) dir.create(out_dir,recursive=TRUE)
base<-cevam_choose_design(.20,.40,c(10,15,20,25,30,35,40),.10,.88,"CEVAM",criterion="EN0")

backward_budget <- function(p,d) {
  N<-tail(d$looks,1L);v<-matrix(0,N+1,N+1); if(d$terminal<=N) v[N+1,(d$terminal+1):(N+1)]<-1
  rules<-setNames(seq_along(d$lower),d$looks[-length(d$looks)])
  for(n in (N-1):0) {
    v[n+1,1:(n+1)]<-(1-p)*v[n+2,1:(n+1)]+p*v[n+2,2:(n+2)]
    key<-as.character(n)
    if(key %in% names(rules)) {j<-rules[[key]];l<-d$lower[j];u<-d$upper[j];if(l>=0)v[n+1,1:(l+1)]<-0;if(u<=n)v[n+1,(u+1):(n+1)]<-1}
  }
  v
}
replacement <- function(q,r,p0,p1,maxm) {
  if(q<1e-14&&r<1e-14)return(c(m=0,c=1,size=0,power=0));if(q>1-1e-14&&r>1-1e-14)return(c(m=0,c=0,size=1,power=1))
  for(m in 1:maxm) for(c in 0:(m+1)) {
    s<-if(c<=m)pbinom(c-1,m,p0,lower.tail=FALSE) else 0;pw<-if(c<=m)pbinom(c-1,m,p1,lower.tail=FALSE) else 0
    if(s<=q-1e-12&&pw>=r+1e-12)return(c(m=m,c=c,size=s,power=pw))
  };NULL
}
adaptive_oc <- function(p,p0,p1,d,q,r,extra=c(18,22,27),cap=60L) {
  N<-tail(d$looks,1L);rules<-setNames(seq_along(d$lower),d$looks[-length(d$looks)]);alive<-1;en<-rej<-adapt<-0
  policies<-list()
  for(n in 1:N) {
    en<-en+sum(alive);v<-numeric(length(alive)+1L);v[1:length(alive)]<-v[1:length(alive)]+alive*(1-p);v[2:(length(alive)+1L)]<-v[2:(length(alive)+1L)]+alive*p
    key<-as.character(n);if(key%in%names(rules)){j<-rules[[key]];u<-d$upper[j];l<-d$lower[j];if(u<=n){rej<-rej+sum(v[(u+1):(n+1)]);v[(u+1):(n+1)]<-0};if(l>=0)v[1:(l+1)]<-0}
    if(n%in%extra) for(x in 0:n) if(v[x+1]>0 && q[n+1,x+1]>=.01&&q[n+1,x+1]<=.30&&r[n+1,x+1]>=.20&&r[n+1,x+1]<=.95) {
      plan<-replacement(q[n+1,x+1],r[n+1,x+1],p0,p1,cap-n);if(!is.null(plan)){m<-plan['m'];c<-plan['c'];tailp<-if(c<=m)pbinom(c-1,m,p,lower.tail=FALSE)else 0;rej<-rej+v[x+1]*tailp;en<-en+v[x+1]*m;adapt<-adapt+v[x+1];policies[[length(policies)+1L]]<-c(n=n,x=x,plan);v[x+1]<-0}
    }
    if(n==N){if(d$terminal<=N) rej<-rej+sum(v[(d$terminal+1):(N+1)]);v[]<-0};alive<-v
  }
  list(rejection=rej,EN=en,adaptation_probability=adapt,policies=policies)
}
q<-backward_budget(.20,base);r<-backward_budget(.40,base)
rows<-list();k<-0L
for(p in c(0,.05,.10,.15,.20,.30,.40,.50,.60,1)) {z<-adaptive_oc(p,.20,.40,base,q,r);orig<-cevam_exact_oc(p,base);k<-k+1L;rows[[k]]<-data.frame(p=p,original_rejection=orig$rejection,original_EN=orig$EN,repaired_rejection=z$rejection,repaired_EN=z$EN,adaptation_probability=z$adaptation_probability)}
write.csv(do.call(rbind,rows),file.path(out_dir,"supp_adaptation_exact.csv"),row.names=FALSE)
saveRDS(list(base=base,q=q,r=r),file.path(out_dir,"supp_adaptation_budgets.rds"))
