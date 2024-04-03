library(CML)
library(graph)
library(bnlearn)
data("asiaDAG")
data("asiadf")
local_est <- cml(true_dag = asiaDAG,targets = c(1,6),
                 node_names = colnames(asiaDAG),verbose = FALSE)
plotOutput(local_est,asiaDAG)

sample_local_est <- cml(data = asiadf,targets = c(1,6),node_names = colnames(asiaDAG),
    lmax = 3,tol = 0.01,mb_tol = 0.05,method = "MMPC",
    test = "testIndFisher",verbose=FALSE)

pa_sets <- list(
  list(c(),c(2))
)

jointIda(c(1,6),8,sample_local_est$data_cov,all.pasets = pa_sets,technique = "RRC")
jointIda(c(1,6),8,sample_local_est$data_cov,graphEst = sample_local_est$amat,technique = "RRC",type = "dag")

g <- graphNEL(nodes=colnames(asiaDAG)[sample_local_est$Nodes],edgemode = "undirected")
g <- addEdge("asia","tub",g,0)
g <- addEdge("tub","either",g,0)
g <- addEdge("either","tub",g,0)
plot(g)

