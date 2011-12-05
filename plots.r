library(ggplot2)
library(reshape)

jll = read.csv("jtm-5000.csv", header=FALSE)
mll = read.csv("mtm-5000.csv", header=FALSE)
colnames(jll) = c("iteration", "j")
colnames(mll) = c("iteration", "m")
ll5k = melt(merge(jll,mll,by="iteration"), id="iteration")
colnames(ll5k) = c("iteration", "jorm", "loglikelihood")
qplot(iteration, loglikelihood, data=ll5k, color=jorm)


lda_trajectory = function(csvfile) {
  df = read.csv(csvfile, header=FALSE)
  colnames(df) = c("iteration", "train", "test")
  ll = melt(df, id="iteration")
  colnames(ll) = c("iteration", "split", "loglikelihood")
  ll
}


lda_log_trajectory = function(csvfile) {
  df = read.csv(csvfile, header=FALSE)
  colnames(df) = c("iteration", "train")
  df
}


ll = lda_trajectory("per-token-ll.csv")
qplot(iteration, loglikelihood, data=ll, color=split)
qplot(iteration, loglikelihood, data=ll[ll$iteration > 300,], color=split)



  


dmrll = lda_trajectory("dmr-1000.csv")
qplot(iteration, loglikelihood, data=dmrll, color=split)

z4dmr = lda_trajectory("resources/dmr-synthetic-1000-iterations-4-topics-1-threads-dmr-jkan/dmr.csv")
qplot(iteration, loglikelihood, data=z4dmr[z4dmr$iteration > 800,], color=split)

j4dmr = lda_trajectory("resources/dmr-synthetic-1000-iterations-4-topics-1-threads-dmr-jkan/dmr-2.csv")
qplot(iteration, loglikelihood, data=j4dmr[j4dmr$iteration > 400,], color=split)
j4dmr = lda_trajectory("resources/dmr-synthetic-1000-iterations-4-topics-1-threads-dmr-jkan/dmr-3.csv")

logdmr = lda_log_trajectory("resources/dmr-synthetic-1000-iterations-4-topics-1-threads-dmr-jkan/dmr-log.csv")
qplot(iteration, train, data=logdmr)



twnglda = lda_log_trajectory("twng.csv")

twnglda = lda_trajectory("resources/twenty-newsgroups-1000-iterations-20-topics-2.500000-alpha-0.010000-beta-1-threads-mallet/eval.csv")
qplot(iteration, loglikelihood, data=twnglda, color=split)
qplot(iteration, loglikelihood, data=twnglda[twnglda$iteration > 300,], color=split)
