
# Setup -------------------------------------------------------------------
library(bnlearn)
data("asiadf")
data("asiaDAG")
node_names <- colnames(asiaDAG)
asiaDAG <- matrix(asiaDAG,nrow = ncol(asiadf),ncol = ncol(asiadf))
p <- length(node_names)

test_that("Wrapper function works (Sample with true DAG)",{
  # CML
  results <- cml(true_dag = asiaDAG,data = asiadf,targets = c(1,6),node_names=node_names,verbose = FALSE)
  # g <- empty.graph(node_names)
  # amat(g) <- results$amat
  # graphviz.plot(g)
  for (result_part in setdiff(names(results),c("totalSkeletonTime","targetSkeletonTimes","mbEstTime","totalTime","MBNumTests"))){
    expect_snapshot_output(print(results[[result_part]]))
  }
  expect_equal(results[["MBNumTests"]],0)
  
  # SNL
  results <- snl(true_dag = asiaDAG,data = asiadf,targets = c(1,6),node_names=node_names,verbose = FALSE)
  # g <- empty.graph(node_names)
  # amat(g) <- results$amat
  # graphviz.plot(g)
  for (result_part in setdiff(names(results),c("targetSkeletonTimes","totalTime","mbEstTime","MBNumTests"))){
    expect_snapshot_output(print(results[[result_part]]))
  }
  expect_equal(results[["MBNumTests"]],0)
})

test_that("Wrapper function works (Population)",{
  # CML
  results <- cml(true_dag = asiaDAG,targets = c(1,6),node_names=node_names,verbose = FALSE)
  # g <- empty.graph(node_names)
  # amat(g) <- results$amat
  # graphviz.plot(g)
  for (result_part in setdiff(names(results),c("totalSkeletonTime","targetSkeletonTimes","mbEstTime","totalTime","MBNumTests"))){
    expect_snapshot_output(print(results[[result_part]]))
  }
  expect_equal(results[["MBNumTests"]],0)
  
  # SNL
  results <- snl(true_dag = asiaDAG,targets = c(1,6),node_names=node_names,verbose = FALSE)
  # g <- empty.graph(node_names)
  # amat(g) <- results$amat
  # graphviz.plot(g)
  for (result_part in setdiff(names(results),c("targetSkeletonTimes","totalTime","mbEstTime","MBNumTests"))){
    expect_snapshot_output(print(results[[result_part]]))
  }
  expect_equal(results[["MBNumTests"]],0)
  
})

test_that("Wrapper function works (Sample)",{
  # CML
  results <- cml(data = asiadf,targets = c(1,6),node_names=node_names,verbose = FALSE)
  # g <- empty.graph(node_names)
  # amat(g) <- results$amat
  # graphviz.plot(g)
  for (result_part in setdiff(names(results),c("totalSkeletonTime","targetSkeletonTimes","totalTime","mbList","mbEstTime","MBNumTests"))){
    expect_snapshot_output(print(results[[result_part]]))
  }
  results1 <- results[["MBNumTests"]]
  # SNL
  results <- snl(data = asiadf,targets = c(1,6),node_names=node_names,verbose = FALSE)
  # g <- empty.graph(node_names)
  # amat(g) <- results$amat
  # graphviz.plot(g)
  for (result_part in setdiff(names(results),c("targetSkeletonTimes","totalTime","mbList","MBNumTests","mbEstTime"))){
    expect_snapshot_output(print(results[[result_part]]))
  }
  expect_snapshot_output(print(results1))
  expect_snapshot_output(print(results[["MBNumTests"]]))
})

test_that("Testing pre-checks",{
  expect_error(cml(data = asiadf,targets = c(1,6),lmax = -1,node_names=node_names,verbose = FALSE))
  expect_error(snl(data = asiadf,targets = c(1,6),lmax = -1,node_names=node_names,verbose = FALSE))
})

#Need to convert to integer
test_that("Discrete version",{
  asia_mat <- data.matrix(asia)-1
  
  asia_d_cml <- cml(asia_mat, targets = c(2,5,6), test = "gSquare",verbose = FALSE)
  expect_snapshot_output(asia_d_cml$amat)
  expect_snapshot_output(asia_d_cml$S)
  expect_snapshot_output(asia_d_cml$RulesUsed)
  
  #Graphing
  # data("asiaDAG")
  # node_names <- colnames(asiaDAG)
  # p <- ncol(asiaDAG)
  # 
  # asia_g <- empty.graph(node_names)
  # amat(asia_g) <- asia_d_cml$amat
  
  #plotOutput(asia_d_cml,asiaDAG)
})






