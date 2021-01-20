source("InstallPackages.R")

## Credit all goes to Kelly!!!

count.vector <- function(q, q.dim){
  # This function returns a categorical count vector 
  #
  # Args:
  #   q - list of factor variables
  #   q.dim - number of factor levels
  q2 = as.data.frame(table(q))
  counts <- matrix(0, nrow = q.dim)
  counts[as.numeric(as.character(q2$q))] = q2$Freq
  return(counts)
}

generate.mmlda.data <- function(K, D, J, I, lam=300, alp=NA, sig=NA, mu=NA){
  # This function generates test data for MM-LDA 
  #
  # Args:
  #   K (>= 2): Number of factors
  #   D: Number of documents
  #   J: vocabulary count
  #   I: number of distinct image regions
  #   lam: Avg. number of words/regions per document
  #   alp, sig, mu: Dirichlet hyperparameter values
  # Check parameters
  if (K < 2) {stop("'K' must be at least 2")}
  if (D < 1) {stop("'D' must be at least 1")}
  if (J < 2) {stop("'J' must be at least 2")}
  if (I < 2) {stop("'I' must be at least 2")}
  if (lam < 10) {stop("'lam' must be at least 10")}
  if (any(is.na(alp))) {alp = matrix(1, nrow = K, ncol = 1)}
  if (any(is.na(sig))) {sig = matrix(1, nrow = I, ncol = 1)}
  if (any(is.na(mu))) {mu = matrix(1, nrow = J, ncol = 1)}
  if (any(alp <= 0)) {stop("'alpha' must be greater than 0.")}
  if (any(sig <= 0)) {stop("'sigma' must be greater than 0.")}
  if (any(mu <= 0)) {stop("'mu' must be greater than 0.")}
  # Create true parameters
  theta <- drop(replicate(D, MCMCpack::rdirichlet(1, alpha=alp)))
  beta <- drop(replicate(K, MCMCpack::rdirichlet(1, alpha=sig)))
  phi <- drop(replicate(K, MCMCpack::rdirichlet(1, alpha=mu)))
  # Create document data
  R <- Matrix::Matrix(0, nrow = D, ncol = I, sparse = TRUE, byrow = TRUE)  # img
  W <- Matrix::Matrix(0, nrow = D, ncol = J, sparse = TRUE, byrow = TRUE) # word
  for (d in 1:D){
    # Number of words & image regions within document
    n.word <- rpois(1, lambda = lam)
    n.reg <- rpois(1, lambda = lam)
    # Sample labels
    vocab.topic.count <- count.vector(sample(1:K, size=n.word, replace=TRUE, prob=theta[,d]), K)
    region.topic.count <- count.vector(sample(1:K, size=n.reg, replace=TRUE, prob=theta[,d]), K)
    # Generates words/images for each topic...
    Rk <- sapply(1:K, function(k){count.vector(sample(1:I, size=region.topic.count[k], replace=TRUE, prob=beta[,k]), I)})
    Wk <- sapply(1:K, function(k){count.vector(sample(1:J, size=vocab.topic.count[k], replace=TRUE, prob=phi[,k]), J)})
    W[d,] <- rowSums(Wk)
    R[d,] <- rowSums(Rk)
  }
  return(list(theta=t(theta), beta=t(beta), phi=t(phi), W=W, R=R, lam=lam, 
              alp=alp, sig=sig, mu=mu))
}

lda.get.instances <- function(W){
  # This extracts coordinate and values of nonzero values in W
  #
  # Args:
  #   W - (D x J) dgCMatrix/matrix of word counts
  #
  # Return:
  #   doc.id - vector of doc IDs instances
  #   feat.id - vector of feature IDs instances
  #   total.count - total number of feature instances
  #print("ENTERS GET.INSTANCES :)")
  #print(paste("The class of W is ",class(W)))
  #print(W)
  D = dim(W)[1]
  #print(paste("D =",D))
  J = dim(W)[2]
  #print(paste("J =",J))
  total.count = sum(W)
  #print(paste("total count is", total.count))
  doc.id = rep(0, total.count)
  feat.id = rep(0, total.count)
  extract <- Matrix::summary(W)
  #print(extract)
  #nonzero.entries.count <- nrow(extract)
  #nonzero.entries.count1 <- dim(extract)[1]
  #nonzero.entries.count2 <- length(extract)
  #nonzero.entries.count3 <- nrow(extract[,1])
  nonzero.entries.count <- Matrix::nnzero(W)
  #print(paste("num. of nonzero entries:", nonzero.entries.count))
  #print(paste("num. of nonzero entries1:", nonzero.entries.count1))
  #print(paste("num. of nonzero entries2:", nonzero.entries.count2))
  #print(paste("num. of nonzero entries3:", nonzero.entries.count3))
  #print(paste("num. of nonzero entries4:", nonzero.entries.count4))
  idx = 1
  for (ii in 1:nonzero.entries.count){
    doc.id[idx:(idx + extract$x[ii] - 1)] = rep(extract$i[ii], extract$x[ii])
    feat.id[idx:(idx + extract$x[ii] - 1)] = rep(extract$j[ii], extract$x[ii])
    idx = idx + extract$x[ii]
  }
  #print(doc.id)
  #print(feat.id)
  return(list(doc.id=doc.id, feat.id=feat.id, total.count=total.count))
}

simdata = generate.mmlda.data(K=K,D=D,I=I,J=J,lam=lam)

datatable=as.data.frame(as.matrix(simdata$W))
w.vals = lda.get.instances(simdata$W)
