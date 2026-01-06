##################################################
#                                                #
#                    3D_plots                    #
#                                                #
##################################################


# ============== - Input Variables - =======================

Visafree_network <- Visafree_network
Dya_GeoDistance <- Dya_GeoDistance
Dya_GeoDistanceNorm <- Dya_GeoDistanceNorm
Dya_RelDistance <- Dya_RelDistance
Dya_Border <- Dya_Border

# ============== - FinalModel - =======================

# LSM16 <- ergmm(Visafree_network ~ euclidean(d = 3, G = 2)
#                + rsender
#                + rreceiver
#                , control=ergmm.control(store.burnin=TRUE))

summary(LSM16)

par(par_temp)
par(mfrow=c(2,3))
plot(LSM16_gof)

plot(LSM16,use.rgl=FALSE, labels = TRUE)
plot(LSM16,use.rgl=FALSE, labels = TRUE, what = "pmean")
plot(LSM16,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.1)
plot(LSM16,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.01
     ,rand.eff="sender", rand.eff.cap	= 10)
plot(LSM16,use.rgl=TRUE, labels = TRUE, 
     edge.plot3d = FALSE, vertex.3d.cex = 0.01
     ,rand.eff="receiver", rand.eff.cap	= 10)

# ---- Plot using plot_ly ----
Coordinates <- as.data.frame(LSM16$mkl$Z)
names(Coordinates) <- c("Z1","Z2","Z3")
Coordinates <- cbind(Coordinates, VertexName, Vertex_RealNames,
                     LSM16[["mkl"]][["receiver"]], LSM16[["mkl"]][["sender"]],
                     LSM16[["mkl"]][["Z.K"]])
names(Coordinates) <- c("Z1","Z2","Z3", "VertexName","Vertex_RealNames",
                        "Receiver", "Sender", "Group")


ModelNAtts <- merge(Coordinates, VertexAttributes, by = "VertexName", all.x = TRUE)
ModelNAtts$GDP <- round(exp(ModelNAtts$ln_GDP)/1000000)
ModelNAtts$Area <- ((ModelNAtts$Transformed_Area)^9)/1000000
ModelNAtts$NetMigration <- ((ModelNAtts$Trans_NetMigration)^5)


plot_ly(Coordinates, x = ~Z1, y = ~Z2, z = ~Z3
        , size = ~Sender
        , marker = list(symbol = 'circle', sizemode = 'diameter')
        , sizes = c(5, 50)
)



plot_ly(ModelNAtts, x = ~Z1, y = ~Z2, z = ~Z3, color = ~VertexName)

# Visualisering med GDP
plot_ly(ModelNAtts, x = ~Z1, y = ~Z2, z = ~Z3, alpha = 0.7,
        text =  ~paste('Country:', Vertex_RealNames,
                       '<br>ISO3:', VertexName 
                       ,'<br>GDP (milj. dollar):', GDP
        ),
        marker = list(color = ~ln_GDP, 
                      colorscale = c('#FFE1A1', '#683531'), 
                      showscale = TRUE))

# Visualisering med Area
plot_ly(ModelNAtts, x = ~Z1, y = ~Z2, z = ~Z3, alpha = 0.7, 
        text =  ~paste('Country:', Vertex_RealNames,
                       '<br>ISO3:', VertexName 
                       ,'<br>GDP (milj. dollar):', GDP
                       ,'<br>Area (milj):', Area
        ),
        marker = list(color = ~Transformed_Area, 
                      colorscale = c('#FFE1A1', '#683531'), 
                      showscale = TRUE))

# Visualisering med NonViolence
plot_ly(ModelNAtts, x = ~Z1, y = ~Z2, z = ~Z3, alpha = 0.7, 
        text =  ~paste('Country:', Vertex_RealNames,
                       '<br>ISO3:', VertexName 
                       ,'<br>GDP (milj. dollar):', GDP
                       ,'<br>Area (milj):', Area
                       ,'<br>NonViolence:', NonViolence
                       ,'<br>Receiver:', Receiver
                       ,'<br>Sender:', Sender
        ),
        marker = list(color = ~NonViolence, 
                      colorscale = c('#FFE1A1', '#683531'), 
                      showscale = TRUE
                      , symbol = 'circle', sizemode = 'diameter')
        , size = ~Sender
        , sizes = c(5, 50)
)

# Visualisering med NetMigration
plot_ly(ModelNAtts, x = ~Z1, y = ~Z2, z = ~Z3, alpha = 0.7, 
        text =  ~paste('Country:', Vertex_RealNames,
                       '<br>ISO3:', VertexName 
                       ,'<br>GDP (milj. dollar):', GDP
                       ,'<br>Area (milj):', Area
                       ,'<br>NetMigration:', NetMigration
        ),
        marker = list(color = ~NetMigration, 
                      colorscale = c('#FFE1A1', '#683531'), 
                      showscale = TRUE))

# Visualisering med Group
plot_ly(ModelNAtts, x = ~Z1, y = ~Z2, z = ~Z3, alpha = 0.7,
        color = ~Group, colors = c('#FF2500', '#008700'),
        text =  ~paste('Country:', Vertex_RealNames,
                       '<br>ISO3:', VertexName 
                       ,'<br>GDP (milj. dollar):', GDP
                       ,'<br>Area (milj):', Area
        ))

# Visualisering med Coloniser
plot_ly(ModelNAtts, x = ~Z1, y = ~Z2, z = ~Z3, alpha = 0.7,
        color = ~Coloniser, colors = c('#BF382A', '#0C4B8E'),
        text =  ~paste('Country:', Vertex_RealNames,
                       '<br>ISO3:', VertexName 
                       ,'<br>GDP (milj. dollar):', GDP
                       ,'<br>Area (milj):', Area
                       ,'<br>Coloniser:', Coloniser
        ))

# Visualisering med Colonised
plot_ly(ModelNAtts, x = ~Z1, y = ~Z2, z = ~Z3, alpha = 0.7,
        color = ~Colonised, colors = c('#BF382A', '#0C4B8E'),
        text =  ~paste('Country:', Vertex_RealNames,
                       '<br>ISO3:', VertexName 
                       ,'<br>GDP (milj. dollar):', GDP
                       ,'<br>Area (milj):', Area
                       ,'<br>Colonised:', Colonised
        ))

colors <- c('#4AC6B7', '#4912A4', '#565F8A', '#FF7070', '#C61951',
            '#965F8A', '#197224', '#C619F1', '#1619F1')
# Visualisering med LangGroup
plot_ly(ModelNAtts, x = ~Z1, y = ~Z2, z = ~Z3, alpha = 0.7,
        color = ~LangGroup, colors = colors,
        text =  ~paste('Country:', Vertex_RealNames,
                       '<br>ISO3:', VertexName 
                       ,'<br>GDP (milj. dollar):', GDP
                       ,'<br>Area (milj):', Area
                       ,'<br>LangGroup:', LangGroup
        ))

colors67 <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd",
              "#8c564b","#e377c2", "#7f7f7f","#bcbd22","#17becf",
              "#393b79","#637939","#8c6d31","#843c39", "#7b4173",
              "#5254a3","#6b6ecf","#9c9ede","#8ca252","#b5cf6b",
              "#cedb9c", "#bd9e39","#e7ba52","#e7cb94","#ad494a",
              "#d6616b","#e7969c","#a55194", "#ce6dbd","#de9ed6",
              "#3182bd","#6baed6","#9ecae1","#e6550d","#fd8d3c",
              "#fdae6b","#fdd0a2","#31a354","#74c476","#a1d99b",
              "#c7e9c0","#756bb1", "#9e9ac8","#bcbddc","#dadaeb",
              "#636363","#969696","#bdbdbd","#d9d9d9", "#e41a1c",
              "#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33",
              "#a65628", "#f781bf","#999999","#66c2a5","#fc8d62",
              "#8da0cb","#e78ac3","#a6d854", "#ffd92f","#e5c494",
              "#b3b3b3",'#C619F1')
# Visualisering med Language
plot_ly(ModelNAtts, x = ~Z1, y = ~Z2, z = ~Z3, alpha = 0.7,
        color = ~Language, colors = colors67,
        text =  ~paste('Country:', Vertex_RealNames,
                       '<br>ISO3:', VertexName 
                       ,'<br>GDP (milj. dollar):', GDP
                       ,'<br>Area (milj):', Area
                       ,'<br>Language:', Language
        ))




# Visualisering av sender/receiver med Area
plot_ly(ModelNAtts, x = ~Sender, y = ~Receiver, z = ~1, alpha = 0.7, 
        text =  ~paste('Country:', Vertex_RealNames,
                       '<br>ISO3:', VertexName 
                       ,'<br>GDP (milj. dollar):', GDP
                       ,'<br>Area (milj):', Area
        ),
        marker = list(color = ~Transformed_Area, 
                      colorscale = c('#FFE1A1', '#683531'), 
                      showscale = TRUE))


plot(ModelNAtts$Sender, ModelNAtts$Receiver, labels = TRUE)

# Visualisering av sender/receiver med Group
plot_ly(ModelNAtts, x = ~Sender, y = ~Receiver, z = ~1, alpha = 0.7,
        color = ~Group, colors = c('#BF382A', '#0C4B8E'),
        text =  ~paste('Country:', Vertex_RealNames,
                       '<br>ISO3:', VertexName 
                       ,'<br>GDP (milj. dollar):', GDP
                       ,'<br>Area (milj):', Area
        ))

# Visualisering av sender/receiver med Language
plot_ly(ModelNAtts, x = ~Sender, y = ~Receiver, z = ~1, alpha = 0.7,
        color = ~Language, colors = colors67,
        text =  ~paste('Country:', Vertex_RealNames,
                       '<br>ISO3:', VertexName 
                       ,'<br>GDP (milj. dollar):', GDP
                       ,'<br>Area (milj):', Area
        ))

# Visualisering av GDP/Area med Group
plot_ly(ModelNAtts, x = ~ln_GDP, y = ~Transformed_Area, z = ~1, alpha = 0.7,
        color = ~Group, colors = c('#BF382A', '#0C4B8E'),
        text =  ~paste('Country:', Vertex_RealNames,
                       '<br>ISO3:', VertexName 
                       ,'<br>GDP (milj. dollar):', GDP
                       ,'<br>Area (milj):', Area
        ))

# Visualisering av GDP/Area med Language
plot_ly(ModelNAtts, x = ~ln_GDP, y = ~Transformed_Area, z = ~1, alpha = 0.7,
        color = ~Language, colors = colors67,
        text =  ~paste('Country:', Vertex_RealNames,
                       '<br>ISO3:', VertexName 
                       ,'<br>GDP (milj. dollar):', GDP
                       ,'<br>Area (milj):', Area
        ))

