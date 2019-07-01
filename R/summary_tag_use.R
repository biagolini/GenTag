#' @title Summary color year
#' @description Summary the number of each color tag used per year.
#' @param usedcombinations Pre used combinations.
#' @param yearusedcombinations The year in which the combination was used.
#' @param hide_color Color(s) to be hide in the estimation of remain colors tags present in nature.
#' @return A summary of the numeber of tag colors used by year
#' @examples
#' data(pre_used) # Data example
#' scy(pre_used[,1:4],pre_used[,5], hide_color='EMPTY')
#' @export
scy <- function(usedcombinations, yearusedcombinations, hide_color = NA) {
  usedcombinations <- as.matrix(usedcombinations)
  cores_usadas <- sort(unique(c(usedcombinations)))
  if (!is.na(hide_color[1])) {
    cores_usadas <- setdiff(cores_usadas, hide_color)
  }
  anos_estudados <- sort(unique(c(yearusedcombinations)), decreasing = T)
  matrix_usados <- matrix(0, ncol = length(cores_usadas), nrow = length(anos_estudados))
  colnames(matrix_usados) <- cores_usadas
  rownames(matrix_usados) <- anos_estudados
  for (l in 1:length(anos_estudados)) {
    dados_ano <- subset(usedcombinations, yearusedcombinations ==
                          anos_estudados[l])  # Criar tabela do ano
    for (c in 1:length(cores_usadas)) {
      # fazer as contas para dentro da tabela
      cores_usadas_ano <- c(dados_ano)
      n_cor_usada <- length(which(cores_usadas_ano == cores_usadas[c]))
      matrix_usados[l, c] <- n_cor_usada
    }
  }
  return(matrix_usados)
}
#' @title Estimates remaining color
#' @description Estimates number of remaining color tags in the field
#' @param usedcombinations Pre used combinations.
#' @param yearusedcombinations The year in which the combination was used.
#' @param currentyear Current year.
#' @param yearsurvival An estimation of the proportion of animals that survive between years.
#' @param lifespan Combinations older the lifespan will be automatically disregard.
#' @param hide_color Color(s) to be hide in the estimation of remain colors tags present in nature.
#' @return A estimation of the number of remaining color tags in the field.
#' @examples
#' ## The function is currently defined as
#' data(pre_used) # Data example
#' erc(pre_used[,1:4],pre_used[,5],2019,0.85, hide_color='EMPTY')
#' @export
erc <- function(usedcombinations, yearusedcombinations, currentyear = NA,
                yearsurvival = NA, lifespan = NA, hide_color = NA) {
  # Validar dados
  if (is.na(currentyear)){currentyear <- as.numeric(format(Sys.time(), "%Y"));warning(paste("You do not provide the current year, it was assume as",currentyear))}
  if (is.na(yearsurvival)){stop("You must provide an estimation of year survival")}
  matrix_usados <- scy(usedcombinations = usedcombinations, yearusedcombinations = yearusedcombinations,
                       hide_color = hide_color)
  matrix_restam <- matrix(NA, nrow = length(matrix_usados[, 1]), ncol = length(matrix_usados[1,]))
  colnames(matrix_restam) <- colnames(matrix_usados)
  rownames(matrix_restam) <- rownames(matrix_usados)
  anos_estudados <- sort(unique(c(yearusedcombinations)), decreasing = T)
  # Estimativa de quantas anilhas restam
  for (l in 1:length(matrix_usados[, 1])) {
    time_since_band <- currentyear - anos_estudados[l]
    for (c in 1:length(matrix_usados[1, ])) {
      matrix_restam[l, c] <- round((matrix_usados[l, c] * (yearsurvival^time_since_band)),
                                   0)
    }
  }
  # Caso foi fornecido o lifespan, remover os que ja morreram
  if (!is.na(lifespan)){matrix_restam <- matrix_restam[-which(rownames(matrix_restam) < currentyear - lifespan), ]}
  return(matrix_restam)
}