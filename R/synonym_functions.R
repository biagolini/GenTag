#' @title Empty synonym code
#' @description Find synonyms in a group of colors with code for empty tag.
#' @param row_under_review Color sequence to be analyzed.
#' @param emptyname Code used to define empty code.
#' @param columns_set Range of codes which belong to the same tag region.
#' @return Matrix with all synonym
#' @examples
#' # Example of a full sequence
#' combination<- c('EMPTY','Red','Yellow','Red','Blue','Green')
#' # See synonym for the code group 1 to 3
#' escode(combination,'EMPTY',columns_set=1:3)
#' @export
escode <- function(row_under_review, emptyname = NA, columns_set = NA) {
  if (is.na(emptyname)) {
    print(head(("You must inform the code for empty"))
  }
  if (length(columns_set) < 1) {
    stop("You must inform the set of coluns from the sampe leg")
  }
  row_under_review <- matrix(row_under_review, ncol = length(row_under_review))
  analis <- as.matrix(row_under_review)[, columns_set]
  names(analis) <- colnames((row_under_review)[, columns_set])
  if (sum(analis == emptyname) > 2) {
    stop("Only 0, 1 or 2 emptys allowed per leg")
  }
  if ((length(analis) == 1) | (sum(analis == emptyname) == 0) | (sum(analis ==
                                                                     emptyname) == length(analis))) {
    resposta <- analis
  } else {
    colunas_usadas <- which(!analis == emptyname)
    colunas_vazias <- which(analis == emptyname)
    n_anilhas <- length(analis)
    n_vazio <- length(colunas_vazias)
    n_usado <- n_anilhas - n_vazio
    aux_cobinacoes_possiveis <- rep(emptyname, (((n_vazio + 1) *
                                                   n_usado) + n_vazio))
    for (i in 1:n_usado) {
      aux_cobinacoes_possiveis[(n_vazio + 1) * i] <- analis[colunas_usadas[i]]
    }
    encontre <- seq.int(from = (n_vazio + 1), to = ((n_vazio + 1) *
                                                      n_usado), by = (n_vazio + 1))
    onde_estao_vazio <- seq(1:length(aux_cobinacoes_possiveis))[-encontre]
    n_caixa_cor_aux <- length(aux_cobinacoes_possiveis)
    combinacoes_1 <- NULL
    combinacoes_2 <- NULL
    contador <- 0
    if (n_vazio > 1) {
      for (i in 1:(n_caixa_cor_aux - 1)) {
        for (z in which(1:n_caixa_cor_aux > i)) {
          if (sum(i == encontre) == 1 | sum(z == encontre) ==
              1) {
          } else {
            contador <- contador + 1
            combinacoes_1[contador] <- i
            combinacoes_2[contador] <- z
          }
        }
      }
    } else {
      combinacoes_1 <- seq(from = 1, to = length(aux_cobinacoes_possiveis),
                           by = 2)
      combinacoes_2 <- NULL
    }
    resposta <- matrix(NA, ncol = n_anilhas, nrow = length(combinacoes_1))
    for (z in 1:length(combinacoes_1)) {
      resposta[z, ] <- aux_cobinacoes_possiveis[sort(c(encontre,
                                                       combinacoes_1[z], combinacoes_2[z]))]
    }
    resposta <- unique(resposta)
    colnames(resposta) <- names(analis)
  }
  return(resposta)
}

#' @title Empty synonym combination
#' @description Find synonyms of a sequence with code for empty tag.
#' @param row_under_review Color sequence to be analyzed.
#' @param emptyname Code used to define empty code.
#' @param g1 Group of tags from group 1. For example, in a code of 6 colors for bird tag, tags from 1 to 3 belongs to the left leg, and  tags from 1 to 3 belongs to the right. g1 must to address the left leg, and g2 must to address right leg. Thus: 'g1=1:3' and 'g2=4:6'.
#' @param g2 Group of tags from group 2. For example, in a code of 6 colors for bird tag, tags from 1 to 3 belongs to the left leg, and  tags from 1 to 3 belongs to the right. g1 must to address the left leg, and g2 must to address right leg. Thus: 'g1=1:3' and 'g2=4:6'.
#' @param g3 Group of tags from group 3.
#' @param g4 Group of tags from group 4.
#' @param g5 Group of tags from group 5.
#' @param g6 Group of tags from group 6.
#' @return Matrix with all synonyms
#' @examples
#' # Example of a full sequence
#' combination<- c('EMPTY','Red','Yellow','Red','Blue','Green')  # See synonym for the full sequence
#' escombination(combination,'EMPTY',g1=1:3,g2=4:6)
#' @export
escombination <- function(row_under_review, emptyname, g1 = NA, g2 = NA,
                          g3 = NA, g4 = NA, g5 = NA, g6 = NA) {
  ngroups <- (6 - (is.na(g1[1]) + is.na(g2[1]) + is.na(g3[1]) + is.na(g4[1]) +
                     is.na(g5[1]) + is.na(g6[1])))
  row_under_review <- matrix(row_under_review, ncol = length(row_under_review))
  if (ngroups == 0) {
    stop("You must inform at least one group")
  }
  columns_set1 <- NA
  columns_set2 <- NA
  columns_set3 <- NA
  columns_set4 <- NA
  columns_set5 <- NA
  columns_set6 <- NA
  if (ngroups == 1) {
    columns_set1 <- escode(row_under_review, emptyname, columns_set = g1)
  }
  if (ngroups == 2) {
    columns_set1 <- escode(row_under_review, emptyname, columns_set = g1)
    columns_set2 <- escode(row_under_review, emptyname, columns_set = g2)
  }
  if (ngroups == 3) {
    columns_set1 <- escode(row_under_review, emptyname, columns_set = g1)
    columns_set2 <- escode(row_under_review, emptyname, columns_set = g2)
    columns_set3 <- escode(row_under_review, emptyname, columns_set = g3)
  }
  if (ngroups == 4) {
    columns_set1 <- escode(row_under_review, emptyname, columns_set = g1)
    columns_set2 <- escode(row_under_review, emptyname, columns_set = g2)
    columns_set3 <- escode(row_under_review, emptyname, columns_set = g3)
    columns_set4 <- escode(row_under_review, emptyname, columns_set = g4)
  }
  if (ngroups == 5) {
    columns_set1 <- escode(row_under_review, emptyname, columns_set = g1)
    columns_set2 <- escode(row_under_review, emptyname, columns_set = g2)
    columns_set3 <- escode(row_under_review, emptyname, columns_set = g3)
    columns_set4 <- escode(row_under_review, emptyname, columns_set = g4)
    columns_set5 <- escode(row_under_review, emptyname, columns_set = g5)
  }
  if (ngroups == 6) {
    columns_set1 <- escode(row_under_review, emptyname, columns_set = g1)
    columns_set2 <- escode(row_under_review, emptyname, columns_set = g2)
    columns_set3 <- escode(row_under_review, emptyname, columns_set = g3)
    columns_set4 <- escode(row_under_review, emptyname, columns_set = g4)
    columns_set5 <- escode(row_under_review, emptyname, columns_set = g5)
    columns_set6 <- escode(row_under_review, emptyname, columns_set = g6)
  }
  if (length(columns_set1) > length(g1)) {
    n1 <- length(columns_set1[, 1])
  } else {
    n1 <- 1
  }
  if (length(columns_set2) > length(g2)) {
    n2 <- length(columns_set2[, 1])
  } else {
    n2 <- 1
  }
  if (length(columns_set3) > length(g3)) {
    n3 <- length(columns_set3[, 1])
  } else {
    n3 <- 1
  }
  if (length(columns_set4) > length(g4)) {
    n4 <- length(columns_set4[, 1])
  } else {
    n4 <- 1
  }
  if (length(columns_set5) > length(g5)) {
    n5 <- length(columns_set5[, 1])
  } else {
    n5 <- 1
  }
  if (length(columns_set6) > length(g6)) {
    n6 <- length(columns_set6[, 1])
  } else {
    n6 <- 1
  }
  nresultado <- n1 * n2 * n3 * n4 * n5 * n6
  tab_aux <- matrix(NA, ncol = ncol(row_under_review), nrow = nresultado)
  l <- 1
  for (gi in 1:n1) {
    for (gii in 1:n2) {
      for (giii in 1:n3) {
        for (giv in 1:n4) {
          for (gv in 1:n5) {
            for (gvi in 1:n6) {
              tab_aux[l, g1] <- if (length(columns_set1) > length(g1)) {
                columns_set1[gi, ]
              } else {
                columns_set1
              }
              tab_aux[l, g2] <- if (length(columns_set2) > length(g2)) {
                columns_set2[gii, ]
              } else {
                columns_set2
              }
              tab_aux[l, g3] <- if (length(columns_set3) > length(g3)) {
                columns_set3[giii, ]
              } else {
                columns_set3
              }
              tab_aux[l, g4] <- if (length(columns_set4) > length(g4)) {
                columns_set4[giv, ]
              } else {
                columns_set4
              }
              tab_aux[l, g5] <- if (length(columns_set5) > length(g5)) {
                columns_set5[gv, ]
              } else {
                columns_set5
              }
              tab_aux[l, g6] <- if (length(columns_set6) > length(g6)) {
                columns_set6[gvi, ]
              } else {
                columns_set6
              }
              l <- l + 1
            }
          }
        }
      }
    }
  }
  tab_aux <- unique(tab_aux)
  colnames(tab_aux) <- names(row_under_review)
  return(tab_aux)
}

#' @title Empty synonym dataset
#' @description Find synonyms in a dataset contain combination with code for empty tag.
#' @param tag_sheet Dataset contain sequences to be analyzed.
#' @param emptyname Code used to define empty code.
#' @param g1 Group of tags from group 1. For example, in a code of 6 colors for bird tag, tags from 1 to 3 belongs to the left leg, and  tags from 1 to 3 belongs to the right. g1 must to address the left leg, and g2 must to address right leg. Thus: 'g1=1:3' and 'g2=4:6'.
#' @param g2 Group of tags from group 2. For example, in a code of 6 colors for bird tag, tags from 1 to 3 belongs to the left leg, and  tags from 1 to 3 belongs to the right. g1 must to address the left leg, and g2 must to address right leg. Thus: 'g1=1:3' and 'g2=4:6'.
#' @param g3 Group of tags from group 3.
#' @param g4 Group of tags from group 4.
#' @param g5 Group of tags from group 5.
#' @param g6 Group of tags from group 6.
#' @return Matrix with all synonyms for from entire dataset
#' @examples
#' (combination<- matrix(c('EMPTY','Red','Blue','Green'),4,6,TRUE)) # Example of a dataset
#' esdataset(combination,'EMPTY',g1=1:3,g2=4:6)
#' @export
esdataset <- function(tag_sheet, emptyname, g1 = NA, g2 = NA, g3 = NA,
                      g4 = NA, g5 = NA, g6 = NA) {
  para_rodar <- as.matrix(tag_sheet)
  resposta <- as.matrix(tag_sheet)
  for (i in 1:length(para_rodar[, 1])) {
    tab_sin_aux <- escombination(para_rodar[i, ], emptyname, g1,
                                 g2, g3, g4, g5, g6)
    resposta <- unique(rbind(resposta, tab_sin_aux))
  }
  rownames(resposta) <- 1:length(resposta[, 1])
  return(resposta)
}

#' @title  Pre-used combinations combinations
#' @name  pre_used
#' @docType data
#' @usage data(pre_used)
#' @details Simulated database to example of registers of color tag  sequences usage. Columns 1, 2 ,3 and 4 represent the tag code, and column 5 is the date of tagging.
#' @format A data frame with 1200 observations on the following 5 variables.
#' \describe{
#' \item{\code{Tag_1}}{a factor with levels \code{Black} \code{Brown} \code{Dark_Blue} \code{EMPTY} \code{Gray} \code{Green} \code{Light_Blue} \code{Orange} \code{Pink} \code{Red} \code{White} \code{Yellow}}
#' \item{\code{Tag_2}}{a factor with levels \code{Black} \code{Brown} \code{Dark_Blue} \code{Gray} \code{Green} \code{Light_Blue} \code{Metal} \code{Orange} \code{Pink} \code{Red} \code{White} \code{Yellow}}
#' \item{\code{Tag_3}}{a factor with levels \code{Black} \code{Brown} \code{Dark_Blue} \code{EMPTY} \code{Gray} \code{Green} \code{Light_Blue} \code{Orange} \code{Pink} \code{Red} \code{White} \code{Yellow}}
#' \item{\code{Tag_4}}{a factor with levels \code{Black} \code{Brown} \code{Dark_Blue} \code{Gray} \code{Green} \code{Light_Blue} \code{Metal} \code{Orange} \code{Pink} \code{Red} \code{White} \code{Yellow}}
#' \item{\code{Year}}{a numeric vector}
#' }
#' @source Simulated database
#' @keywords datasets
"pre_used"
