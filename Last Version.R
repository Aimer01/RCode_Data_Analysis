#rm(list = ls())


Debi1 = read.csv("Debi_12.csv")
Erosion1 = read.csv("Erosion_12.csv")
Land1 = read.csv("Land_12.csv")
Erosion1 = read.csv("Erosion_12.csv")
LU1 = read.csv("LU_12.csv")
Slope1 = read.csv("Slope_12.csv")


Debi_Label = cbind(Debi1, Erosion1[,11])
Erosion_Label = Erosion1[,c(1:5,11)]
Land_Label = cbind(Land1[,1:6], Erosion1[,11])
LU_Label = cbind(LU1[,1:6], Erosion1[,11])
x = c(1:8) * 3
Slope_Label = cbind(Slope1[,-c(x, (x + 1))], Erosion1[,11])
Slope_Label = Slope_Label[c(3,6,9,12),]


CorP_Debi = cor(Debi_Label[,-(1:2)])
CorP_Debi = CorP_Debi[1:3,]

CorP_Erosion = cor(Erosion_Label[,-(1:2)])
CorP_Erosion = CorP_Erosion[1:3,]

CorP_Land = cor(Land_Label[,-(1:2)])
CorP_Land = CorP_Land[1:4,]

CorP_LU = cor(LU_Label[,-(1:2)])
CorP_LU = CorP_LU[1:4,]

CorP_Slope = cor(Slope_Label[,-(1:2)])
CorP_Slope = CorP_Slope[1:8,]


Feature_Reduction <- function(CorP_Debi) {
  
  DataSetP_Debi = as.data.frame(matrix(0, nrow = 4, ncol = nrow(CorP_Debi)))
  colnames(DataSetP_Debi) = row.names(CorP_Debi)
  row.names(DataSetP_Debi) = c('Independent', 'Weak','Moderate','Strong')
  
  Len = nrow(CorP_Debi)
  for (i in 1:nrow(CorP_Debi)) {
    CorP_Row = as.data.frame(CorP_Debi[i,-c(1:Len)])
    colnames(CorP_Row)[1] = "V1"
    CorP_Row$V1 = ifelse(CorP_Row$V1 < 0, CorP_Row$V1 * -1, CorP_Row$V1)
    
    DataSetP_Debi[1,i] = length(CorP_Row[CorP_Row$V1 <= 0.2,1])
    DataSetP_Debi[2,i] = length(CorP_Row[(CorP_Row$V1 <= 0.4 & CorP_Row$V1 > 0.2),1])
    DataSetP_Debi[3,i] = length(CorP_Row[(CorP_Row$V1 <= 0.7 & CorP_Row$V1 > 0.4),1])
    DataSetP_Debi[4,i] = length(CorP_Row[CorP_Row$V1 > 0.7,1])
    
  }
  
  return(DataSetP_Debi)
}

Data_Debi = Feature_Reduction(CorP_Debi)
Data_Land = Feature_Reduction(CorP_Land)
Data_Erosion = Feature_Reduction(CorP_Erosion)
Data_LU = Feature_Reduction(CorP_LU)
Data_Slope = Feature_Reduction(CorP_Slope)

Debi1 = Debi1[,-3]
Land1 = Land1[,c(1,2,5)]
LU1 = LU1[,c(1,2,6)]
Slope1 = Slope_Label[,c(1:4,6,8:10)]

Dataset12 = cbind(Debi1, cbind(Land1, cbind(Erosion1, LU1)))

write.csv(Erosion1, "Data//Erosion_Result.csv", row.names = FALSE)
write.csv(Debi1, "Data//Debi_Result.csv", row.names = FALSE)
write.csv(Land1, "Data//Land_Result.csv", row.names = FALSE)
write.csv(LU1, "Data//LU_Result.csv", row.names = FALSE)
write.csv(Slope1, "Data//Slope1_Result.csv", row.names = FALSE)







