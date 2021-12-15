L1="CCSM4_run1_LandR.CS_fSpredictedBLPWYear2091"
L2="CCSM4_run2_LandR.CS_fSpredictedBLPWYear2091"
L3="CCSM4_run3_LandR.CS_fSpredictedBLPWYear2091"
L4="CCSM4_run4_LandR.CS_fSpredictedBLPWYear2091"
L5="CCSM4_run5_LandR.CS_fSpredictedBLPWYear2091"

R1 <- birdPred$BLPW[[match(L1, names(birdPred$BLPW))]]
R2 <- birdPred$BLPW[[match(L2, names(birdPred$BLPW))]]
R3 <- birdPred$BLPW[[match(L3, names(birdPred$BLPW))]]
R4 <- birdPred$BLPW[[match(L4, names(birdPred$BLPW))]]
R5 <- birdPred$BLPW[[match(L5, names(birdPred$BLPW))]]

S1 <- raster::cellStats(R1, stat = sum, na.omit= TRUE)
S2 <- raster::cellStats(R2, stat = sum, na.omit= TRUE)
S3 <- raster::cellStats(R3, stat = sum, na.omit= TRUE)
S4 <- raster::cellStats(R4, stat = sum, na.omit= TRUE)
S5 <- raster::cellStats(R5, stat = sum, na.omit= TRUE)
testMean91<- (S1+S2+S3+S4+S5)/5
