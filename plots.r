library(ggplot2)
library(reshape)

jll = read.csv("jtm-5000.csv", header=FALSE)
mll = read.csv("mtm-5000.csv", header=FALSE)
colnames(jll) = c("iteration", "j")
colnames(mll) = c("iteration", "m")
ll5k = melt(merge(jll,mll,by="iteration"), id="iteration")
colnames(ll5k) = c("iteration", "jorm", "loglikelihood")
qplot(iteration, loglikelihood, data=ll5k, color=jorm)


ptll = read.csv("per-token-ll.csv", header=FALSE)
colnames(ptll) = c("iteration", "train", "test")
qplot(iteration, train, data=ptll)

ll = melt(ptll, id="iteration")
colnames(ll) = c("iteration", "split", "loglikelihood")
qplot(iteration, loglikelihood, data=ll, color=split)
qplot(iteration, loglikelihood, data=ll[ll$iteration > 300,], color=split)




jdmr = read.csv("dmr-1000.csv", header=FALSE)
colnames(jdmr) = c("iteration", "train", "test")
qplot(iteration, train, data=jdmr)

dmrll = melt(jdmr, id="iteration")
colnames(dmrll) = c("iteration", "split", "loglikelihood")
qplot(iteration, loglikelihood, data=dmrll, color=split)
