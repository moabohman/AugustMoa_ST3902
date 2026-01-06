##################################################
#                                                #
#      DependencyExploration_Reciprocality       #
#                                                #
##################################################


# ============== - Input Variables - =======================

AdjMat <- AdjMat
Visafree_network <- Visafree_network

# ========== - Network properties - ================

# Number of nodes
n <- network.size(Visafree_network)
# Network density
Visafree_density <- gden(Visafree_network)
# All possible dyads
dyads_all <- (n*(n-1))/2
# Verifying no loops, since the notion of mutuality 
# is well-defined only for dyads (sand, s.54)
has.loops(Visafree_network)


# ============== - Dyad census analysis - =======================


# Examine dyad census
dyad.census(Visafree_network)

# Mut Asym  Null
# 1042 5852 12807


# Calculate expected dyad census
p <- Visafree_density
# Mutual        800
(p^2)*dyads_all
# Asymmetric    6338 
2*p*(1-p)*dyads_all
# Null          12564
((1-p)^2)*dyads_all


# One may already in this step use a Chi-square goodness-of-fit test
# to conclude that 
# H_0 = The dyad census for the observed network comes from randomness only
# does not hold.
# But one may also show it from simulations:

# --- Bernoulli (“Erdős/Rényi”) model ---
Bernoulli_LR <- glm(Visafree ~ 1, family = binomial, data = TidyData)
Bernoulli_ERGM <- ergm(Visafree_network ~ edges)

summary(Bernoulli_LR)
summary(Bernoulli_ERGM)
# Identical!
# And with beta_0 = -1.37750 representing p=0.2014
exp(-1.37750)/(1+exp(-1.37750))


# Simulate networks from the new model
n_sim <- 100
simulations <- simulate(Bernoulli_ERGM, nsim=n_sim)

# Compute average dyad census across simulations
avg_census_mutual_Indep <- Reduce("+", lapply(simulations, dyad.census)) / n_sim
avg_census_mutual_Indep

rm(simulations)

# Mut     Asym    Null
# 791.72 6276.08 12633.2



# --- Reciprocal dependency model ---
summary(Visafree_network ~ edges + mutual)
Dependency_ERGM <- ergm(Visafree_network ~ edges + mutual)

summary(Bernoulli_ERGM)
summary(Dependency_ERGM)
# Statistically significant mutuality effect!
# The conditional log-odds of a mutual tie are
LogOdds_Mutual <- -1.47628+0.45026                      # -1.02602
# The conditional probability that a tie exists, 
# given that the tie in the reverse direction exists, 
# is about
exp(LogOdds_Mutual)/(1+exp(LogOdds_Mutual))             # 0.2638564
# (when under independency the same probability is p^2 = 0.04056643)


# Simulate networks from the new model
n_sim <- 100
simulations <- simulate(Dependency_ERGM, nsim=n_sim)

# Compute average dyad census across simulations
avg_census_mutual_Dep <- Reduce("+", lapply(simulations, dyad.census)) / n_sim
avg_census_mutual_Dep

rm(simulations)

# Mut     Asym     Null
# 1050.82 5867.19 12782.99

# Much better fit!


# --- Compare gof as well ---

# Bernoulli_ERGM_gof <- gof(Bernoulli_ERGM)
# Dependency_ERGM_gof <- gof(Dependency_ERGM)

Bernoulli_ERGM_gof <- gof(Bernoulli_ERGM, GOF = ~idegree + odegree + distance 
                          + espartners + dspartners
                          + mutual)
Dependency_ERGM_gof <- gof(Dependency_ERGM, GOF = ~idegree + odegree + distance 
                           + espartners + dspartners
                           + mutual)

plot(Bernoulli_ERGM_gof)
plot(Dependency_ERGM_gof)



# par_temp <- par()
# par(par_temp)
# par(mfrow=c(3,2))


