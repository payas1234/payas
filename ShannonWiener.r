library(data.table)

ShannonWiener <- function(filePath = file.choose() ) {
  twforest <- fread(filePath)
  # 計算總物種數
  S <- twforest[, sum(individuals) ]
  # 計算個別物種數佔總物種數之比例
  p_i <- twforest[, individuals/S ]
  # 最後加總並算出 Shannon-Wiener (H') 多樣性指數
  H <- -sum(p_i * log(p_i))
  return(H)
}
