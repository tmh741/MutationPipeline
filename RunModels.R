source("InstallPackages.R") #Install Packages

#Make data
K = 3
D = 10
I = 5
J= 5
lam = 50

source("SimulateData.R") # Make data using Kelly's simulate Data Code

source("MutFunctions.R") # Bring in functions to expedite the process

#Run model (Kelly's LDA)
fit = stan(file="kellylda.stan",
           iter=3000,
           data=list(K = K,
                     J = dim(datatable)[2],
                     D = dim(datatable)[1],
                     M = w.vals$total.count,
                     featw = w.vals$feat.id,
                     docw = w.vals$doc.id,
                     alpha = rep(1,K),
                     mu = rep(0.01,dim(datatable)[2])))

#Graphs
stan_rhat(fit)
display = displayvals(fit) # Put parameters in a table to make displays. Function is in MutFunctions.

address=""
plottheta(display,"kellylda",1:D) # Plot theta's and save a .png of the graph. Function is in MutFunctions.
plotphi(display,"kellylda",1:J) # Plot phi's and save a .png of the graph. Function is in MutFunctions.

##RStan Book LDA
# fit2 = stan(file="lda.stan",
#             iter=10000,
#             data=list(K = K,
#                       V = dim(datatable)[2],
#                       M = dim(datatable)[1],
#                       N = w.vals$total.count,
#                       w = w.vals$feat.id,
#                       doc = w.vals$doc.id,
#                       alpha = rep(0.01,K),
#                       beta = rep(1,dim(datatable)[2])))
# 
# display2 = displayvals(fit2)
# plottheta(display2,"lda",1:D,address=address)
# plotphi(display2,"lda",1:J,address=address)
# 
##Poisson Exponential Model
# fit3 = stan(file="poisexpo.stan",
#             iter=6000,
#             data=list(K = K,
#                       V = dim(datatable)[2],
#                       M = dim(datatable)[1],
#                       N = w.vals$total.count,
#                       w = w.vals$feat.id,
#                       doc = w.vals$doc.id,
#                       alpha = rep(0.01,K),
#                       beta = rep(1,dim(datatable)[2])))
# 
# display2 = displayvals(fit2)
# plottheta(display2,"lda",1:D,address=address)
# plotphi(display2,"lda",1:J,address=address)



