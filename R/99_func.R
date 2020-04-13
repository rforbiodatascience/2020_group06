# Define project functions
# ------------------------------------------------------------------------------

foo <- function(x){
  return(2*x)
}


bar <- function(x){
  return(x^2)
}


model<- function(df) {
  lm(weight ~ height, data = df)
}

get_chem_class <- function(x){
  chem_cols <- c("A" = "Hydrophobic", "R" = "Basic", "N" = "Neutral", "D" = "Acidic",
                 "C" = "sulphur", "Q" = "Neutral", "E" = "Acidic", "G" = "Polar",
                 "H" = "Basic", "I" = "Hydrophobic", "L" = "Hydrophobic", "K" = "Basic",
                 "M" = "sulphur", "F" = "Hydrophobic", "P" = "Hydrophobic", "S" = "Polar",
                 "T" = "Polar", "W" = "Hydrophobic", "Y" = "Polar", "V" = "Hydrophobic")
  return(factor(chem_cols[x]))
}