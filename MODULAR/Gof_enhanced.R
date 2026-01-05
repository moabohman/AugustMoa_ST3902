
#################################################
#                                                #
#                  Gof_enhanced                  #
#                                                #
##################################################


LSM16_gof <- gof(LSM16, GOF = ~idegree + odegree + distance + triadcensus + espartners + dspartners)
# plot(gof)
save(LSM16_gof, file = "LSM16_gof.RData")


LSM16_sim <- simulate(LSM16, nsim=100)
LSM16_sim_dyadcensus <- dyad.census(LSM16_sim$networks)

save(LSM16_sim_dyadcensus, file = "LSM16_sim_dyadcensus.RData")
rm(LSM16_sim)


par(mfrow=c(2,2))
hist(LSM16_sim_dyadcensus[,1]
     # , xlim = c(600,1050)    # 1042
     )
abline(v=dyad.census(Visafree_network)[1], col='red')
hist(LSM16_sim_dyadcensus[,2]
     # , xlim = c(5850,6750)    # 5852
     )
abline(v=dyad.census(Visafree_network)[2], col='red')
hist(LSM16_sim_dyadcensus[,3]
     # , xlim = c(12250,12850)  # 12807
     )
abline(v=dyad.census(Visafree_network)[3], col='red')

par(par_temp)

     # , xlim = c(1000,1200)    # 1042
     # , xlim = c(5400,5900)    # 5852
     # , xlim = c(12700,13100)  # 12807

par(par_temp)
# par(mfcol=c(6,3))
par(mfrow=c(2,3))
# par(mfrow=c(3,6))
# par(mfrow=c(4,6))


# plot(LSM1_gof)
# plot(LSM2_gof)

# plot(LSM2_gof)
# plot(LSM10_gof)
# plot(LSM11_gof)

# plot(LSM10_gof)
# plot(LSM14_gof)
# plot(LSM15_gof)
# plot(LSM16_gof)

# plot(LSM26_gof)
# plot(LSM16_gof)
# plot(LSM17_gof)
# plot(LSM18_gof)


# plot(LSM26_gof)
# plot(LSM25_gof)


####################
#                  #
#    For report    #
#                  #
####################


Modell1_gof <- gof(LSM1, GOF = ~idegree + odegree + distance + triadcensus)
Modell2_gof <- gof(LSM2, GOF = ~idegree + odegree + distance + triadcensus)

Modell3_gof <- gof(LSM10, GOF = ~idegree + odegree + distance + triadcensus)
Modell4_gof <- gof(LSM11, GOF = ~idegree + odegree + distance + triadcensus)

Modell5_gof <- gof(LSM14, GOF = ~idegree + odegree + distance + triadcensus)
Modell6_gof <- gof(LSM15, GOF = ~idegree + odegree + distance + triadcensus)
Modell7_gof <- gof(LSM16, GOF = ~idegree + odegree + distance + triadcensus)

Modell8_gof <- gof(LSM17, GOF = ~idegree + odegree + distance + triadcensus)
Modell9_gof <- gof(LSM18, GOF = ~idegree + odegree + distance + triadcensus)

Modell10_gof <- gof(LSM26, GOF = ~idegree + odegree + distance + triadcensus)
Modell11_gof <- gof(LSM25, GOF = ~idegree + odegree + distance + triadcensus)


par(par_temp)
par(mfrow=c(2,2))

plot(Modell1_gof, main = "Goodness-of-fit diagnostics for Model 1")
plot(Modell2_gof, main = "Goodness-of-fit diagnostics for Model 2")
plot(Modell3_gof, main = "Goodness-of-fit diagnostics for Model 3")
plot(Modell4_gof, main = "Goodness-of-fit diagnostics for Model 4")
plot(Modell5_gof, main = "Goodness-of-fit diagnostics for Model 5")
plot(Modell6_gof, main = "Goodness-of-fit diagnostics for Model 6")
plot(Modell7_gof, main = "Goodness-of-fit diagnostics for Model 7")
plot(Modell8_gof, main = "Goodness-of-fit diagnostics for Model 8")
plot(Modell9_gof, main = "Goodness-of-fit diagnostics for Model 9")
plot(Modell10_gof, main = "Goodness-of-fit diagnostics for Model 10")
plot(Modell11_gof, main = "Goodness-of-fit diagnostics for Model 11")





par(mfrow=c(2,2))
hist(LSM25_sim_dyadcensus[,1], main = "Mutual dyads", xlab = "Number of mutual dyads"
     , xlim = c(600,1050)    # 1042
)
abline(v=dyad.census(Visafree_network)[1], col='red')
hist(LSM25_sim_dyadcensus[,2], main = "Assymetric dyads", xlab = "Number of assymetric dyads"
     , xlim = c(5850,6750)    # 5852
)
abline(v=dyad.census(Visafree_network)[2], col='red')
hist(LSM25_sim_dyadcensus[,3], main = "Null dyads", xlab = "Number of null dyads"
     , xlim = c(12250,12850)  # 12807
)
abline(v=dyad.census(Visafree_network)[3], col='red')

mtext( "Modell 11", outer = TRUE, side = 1, font = 2, line = -3.5, 
       cex = 2.4, adj = 0.75, padj = -3.5, col = "#0070C0")




par(par_temp)

# , xlim = c(1000,1200)    # 1042
# , xlim = c(5400,5900)    # 5852
# , xlim = c(12700,13100)  # 12807


mtext( "LSM 24", outer = TRUE, side = 1, font = 2, line = -3.5, cex = 2.4, adj = 0.75, padj = -4.1)
text(x = 0.5, y = 0.5, label = "Title")



# 
# par(mfcol=c(3,2), cex.lab = 2)
# plot(Modell2_gof)
# mtext( "LSM 24", outer = TRUE,font = 2, line = -3.5, cex = 1.4, adj = 0.25)
# plot(gof_LSM22)
# mtext("LSM 22", outer = TRUE,font = 2, line = -3.5, cex = 1.4, adj = 0.765)