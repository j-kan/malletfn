library('ggplot2')
library(reshape)

jll = read.csv("jtm-5000.csv", header=FALSE)
ggplot(jll)

mll = read.csv("mtm-5000.csv", header=FALSE)
ggplot(mll)

ptll = read.csv("per-token-ll.csv", header=FALSE)
colnames(ptll) = c("iteration", "train", "test")
qplot(iteration, train, data=ptll)

ll <- melt(ptll, id="iteration")
colnames(ll) = c("iteration", "split", "loglikelihood")
qplot(iteration, loglikelihood, data=ll, color=split)
qplot(iteration, loglikelihood, data=ll[ll$iteration > 300,], color=split)




