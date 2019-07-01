#' @title All equal tag sequence sample
#' @description Create combinations with equal sample probability to all colors.
#' @param ncombinations Number of combinations to be generated.
#' @param ntag Number of tag to be used in each animal.
#' @param colorsname Names/Code of color tags to be sample.
#' @param gen_method method used for sample colors for tag sequence.
#' @param usedcombinations Pre used combinations.
#' @param colorsf Frequencies/ratio for color sample.
#' @param nspecial Number of special tags/codes, such as metallic, 'EMPTY', or flag (min 0, max 2).
#' @param name1 Name of special tag 1.
#' @param name2 Name of special tag 2.
#' @param location1 Position (or group of positions) to special band 1.
#' @param location2 Position (or group of positions) to special band 2.
#' @param nspecial1 Number of special tag 1 that will be present in all sequences genetated.
#' @param nspecial2 Number of special tag 2 that will be present in all sequences genetated.
#' @param emptyused If pre-used combination has code for empty set as TRUE, otherwise FALSE
#' @param emptyname Code used to define empty code.
#' @param currentyear Current year.
#' @param yearsurvival An estimation of the proportion of animals that survive between years.
#' @param lifespan Combinations older the lifespan will be automatically disregard.
#' @param iotf Ignore older than lifespan. If TRUE ignore pre-used combinations older than lifespan
#' @param yearusedcombinations The year in which the combination was used.
#' @param speed Speed for color frequency adjustment.
#' @param ignorecolor Color to be ignored on lifexp.
#' @param g1 Group of tags from group 1. For example, in a code of 6 colors for bird tag, tags from 1 to 3 belongs to the left leg, and  tags from 1 to 3 belongs to the right. g1 must to address the left leg, and g2 must to address right leg. Thus: 'g1=1:3' and 'g2=4:6'.
#' @param g2 Group of tags from group 2. For example, in a code of 6 colors for bird tag, tags from 1 to 3 belongs to the left leg, and  tags from 1 to 3 belongs to the right. g1 must to address the left leg, and g2 must to address right leg. Thus: 'g1=1:3' and 'g2=4:6'.
#' @param g3 Group of tags from group 3.
#' @param g4 Group of tags from group 4.
#' @param g5 Group of tags from group 5.
#' @param g6 Group of tags from group 6.
#' @param parameterslist parameters for methods not provide by 'GenTag'
#' @return A list of combinations
#' @examples
#' # Create an object contain the name/code of tag colors
#' tcol<-c('Black','Blue','Brown','Gray','Green','Pink','Purple','Red','White','Yellow')
#' # Generate color tag combination without especial tags
#' genseq(30, 4, colorsname= tcol)
#' # Generate color tag combination with especial color (ex metallic tag for numeric identification)
#' genseq(30, 4, tcol, nspecial=1, name1='Metal', location1=c(2,4))
#' # For ongoing works, use the argument usedcombinations to informe the previus used combinations
#' data(pre_used) # Data example
#' genseq(30, 4, colorsname= tcol, usedcombinations=pre_used[,1:4])
#' combinations<-genseq(100, 4, tcol) # save combinations into an object
#' @export
genseq <- function(ncombinations = 100, ntag = 4, colorsname, gen_method = "allequal",
                   usedcombinations = NA, colorsf = NA, nspecial = 0, name1 = "Metal",
                   name2 = "EMPTY", location1 = 1, location2 = 2, nspecial1 = 1, nspecial2 = 1,
                   emptyused = FALSE, emptyname = "EMPTY", currentyear = NA, yearsurvival = 1,
                   lifespan = NA, iotf = FALSE, yearusedcombinations = NA, speed = 1,
                   ignorecolor = NA, g1 = NA, g2 = NA, g3 = NA, g4 = NA, g5 = NA, g6 = NA,
                   parameterslist = NA) {
    
    ### Checking pre-used combinations Checking iotf input data
    if (iotf == TRUE) {
        if (is.na(currentyear)) {
            currentyear <- as.numeric(format(Sys.time(), "%Y"))
            warning(paste("You do not provide the current year, it was assume as",
                          currentyear))
        }
        # Remove older than lifespan
        idade_uso <- currentyear - yearusedcombinations
        usedcombinations <- usedcombinations[which(idade_uso <= lifespan),
                                             ]
    }
    UniqComb <- unique(usedcombinations)
    UniqComb <- as.matrix(UniqComb)
    ncorusada <- length(UniqComb[1, ])
    if (!ncorusada == ntag & !is.na(UniqComb[1, 1])) {
        stop("
Inconsistence: ntag to not match with the number of tags used in pre-used combinations
For further possibilities see: arguments emptyused and emptyname in help(genseq)")
    }
    # Checking for duplicates in pre-used combinations
    if (is.na(UniqComb[1, 1])) {
        UniqComb <- matrix(data = NA, nrow = 1, ncol = ntag)
    } else {
        if (length(UniqComb[, 1]) > 1) {
            tdob <- length(usedcombinations[, 1]) - length(UniqComb[,
                                                                    1])
            if (tdob > 0) {
                stop(paste("Your pre-used data has non-exclusive combinations. The difference between total number of pre used combinations and exclusive is",tdob))
            }
        }
    }
    mcombs <- (length(colorsname)^ntag) - length(UniqComb[, 1])
    # Check if ncombinations is greater than the maximum number of
    # possible combinations
    if (mcombs < ncombinations) {
        stop("Limitation on new possible combinations, ncombinations must be lower than",
             mcombs)
    }
    # Define parameters for combination generation
    if (is.na(parameterslist)) {
        if (gen_method == "allequal") {
            parameterslist <- list(ntag = ntag, colorsname = colorsname,
                                   nspecial = nspecial, name1 = name1, name2 = name2, location1 = location1,
                                   location2 = location2, nspecial1 = nspecial1, nspecial2 = nspecial2)
        } else {
            if (gen_method == "vfrequency") {
                parameterslist <- list(ntag = ntag, colorsf = colorsf,
                                       colorsname = colorsname, nspecial = nspecial, name1 = name1,
                                       name2 = name2, location1 = location1, location2 = location2,
                                       nspecial1 = nspecial1, nspecial2 = nspecial2)
            } else {
                if (gen_method == "lifexp") {
                    if (!length(usedcombinations[, 1]) == length(yearusedcombinations)) {
                        stop("Review your databse, all combinations must have the information of the year")
                    }
                    if (is.na(currentyear)) {
                        currentyear <- as.numeric(format(Sys.time(), "%Y"))
                        warning(paste("You do not provide the current year, it was assume as",
                                      currentyear))
                        
                    }
                    
                    parameterslist <- list(ntag = ntag, colorsname = colorsname,
                                           nspecial = nspecial, name1 = name1, name2 = name2,
                                           location1 = location1, location2 = location2, nspecial1 = nspecial1,
                                           nspecial2 = nspecial2, currentyear = currentyear,
                                           yearsurvival = yearsurvival, lifespan = lifespan,
                                           yearusedcombinations = yearusedcombinations, usedcombinations = usedcombinations,
                                           speed = speed, ignorecolor = ignorecolor)
                } else {
                    stop("For methods out of 'GenTag' package, inform parameters in parameterslist argument")
                }
            }
        }
    }
    # Output
    resposta <- matrix(data = NA, nrow = ncombinations, ncol = ntag)
    # Combinations generator
    while (sum(!is.na(resposta[, 1])) < ncombinations) {
        SComb <- do.call(gen_method, parameterslist)  # Sample a combination
        ftl <- rbind(subset(UniqComb, !is.na(UniqComb[, 1])), subset(resposta,
                                                                     !is.na(resposta[, 1])))
        tag_sheet.test <- rbind(ftl, SComb)
        if (emptyused == TRUE) {
            SiSComb <- escombination(row_under_review = SComb, emptyname = emptyname, g1 = g1, g2 = g2, g3 = g3, g4 = g4, g5 = g5, g6 = g6)
            SiSComb <- as.matrix(SiSComb)
            SComb <- matrix(SComb, ncol = length(SComb))
            SComb_com_sinonimos <- rbind(SComb, SiSComb)
            SComb_com_sinonimos <- unique(SComb_com_sinonimos)
            tag_sheet.test <- rbind(ftl, SComb_com_sinonimos)
        }
        test.duplicada <- sum(duplicated(tag_sheet.test))
        if (test.duplicada == 0) {
            nova.linha <- min(which(is.na(resposta[, 1]) == TRUE))
            resposta[nova.linha, ] <- SComb
        }
    }
    rownames(resposta) <- 1:length(resposta[, 1])
    colnames(resposta) <- paste0("Tag_", 1:length(resposta[1, ]))
    if (length(usedcombinations) > 1) {
        colnames(resposta) <- colnames(usedcombinations)
    }
    return(resposta)
}

#' @title All equal tag sequence sample
#' @description Create combinations with equal sample probability to all colors.
#' @param ntag Number of tag to be used in each animal.
#' @param colorsname Names/Code of color tags to be sample.
#' @param nspecial Number of special tags/codes, such as metallic, 'EMPTY', or flag (min 0, max 2).
#' @param name1 Name of special tag 1.
#' @param name2 Name of special tag 2.
#' @param location1 Position (or group of positions) to special band 1.
#' @param location2 Position (or group of positions) to special band 2.
#' @param nspecial1 Number of special tag 1 that will be present in all sequences genetated.
#' @param nspecial2 Number of special tag 2 that will be present in all sequences genetated.
#' @return A sequencie of tags
#' @examples
#' # Create an object contain the name/code of tag colors
#' tcol<-c('Black','Blue','Brown','Gray','Green','Pink','Purple','Red','White','Yellow')
#' # Generate color tag combination without especial tags
#' genseq(30, 4, colorsname= tcol)
#' # Generate color tag combination with especial color (ex metallic tag for numeric identification)
#' genseq(30, 4, tcol, nspecial=1, name1='Metal',location1=c(2,4))
#' # For ongoing works, use the argument usedcombinations to informe the previus used combinations
#' data(pre_used) # Data example
#' genseq(100, 4, tcol, usedcombinations=pre_used[,1:4])
#' @export
allequal <- function(ntag, colorsname, nspecial = 0, name1 = "Metal",
                     name2 = "EMPTY", location1 = 1, location2 = 2, nspecial1 = 1, nspecial2 = 1) {
    if (nspecial > 2) {
        stop("Maximum of 2 special band allowed")
    }
    colorsname <- as.vector(colorsname)
    if (nspecial == 0) {
        amostra <- sample(x = colorsname, size = ntag, replace = TRUE)
    } else {
        amostra <- rep(NA, ntag)
        if (length(location1) == 1) {
            auxpst <- location1
        } else {
            auxpst <- sample(x = location1, size = nspecial1)
        }
        amostra[auxpst] <- name1
        if (nspecial == 2) {
            sobroup <- intersect(which(is.na(amostra)), location2)
            if (length(sobroup) == 1) {
                auxpst <- sobroup
            } else {
                auxpst <- sample(x = sobroup, size = nspecial2)
            }
            amostra[auxpst] <- name2
        }
        para_pintar <- which(is.na(amostra))
        amostra[para_pintar] <- sample(x = colorsname, size = length(para_pintar),
                                       replace = TRUE)
    }
    amostra <- as.vector(amostra)
    return(amostra)
}
#'@title Variable frequency tag sequence sample
#' @description Create combinations with defined sample probability to each colors.
#' @param ntag Number of tag to be used in each animal.
#' @param colorsname Names/Code of color tags to be sample.
#' @param colorsf Frequencies/ratio for color sample.
#' @param nspecial Number of special tags/codes, such as metallic, 'EMPTY', or flag (min 0, max 2).
#' @param name1 Name of special tag 1.
#' @param name2 Name of special tag 2.
#' @param location1 Position (or group of positions) to special band 1.
#' @param location2 Position (or group of positions) to special band 2.
#' @param nspecial1 Number of special tag 1 that will be present in all sequences genetated.
#' @param nspecial2 Number of special tag 2 that will be present in all sequences genetated.
#' @return A sequencie of tags
#' @examples
#' tcol<-c('Black','Blue','Brown','Gray','Green','Pink','Purple','Red','White','Yellow')
#' p<-c(1,2,5,1,2,2,4,5,8,5)
#' genseq(30, 4, tcol, gen_method='vfrequency', colorsf=p)
#' @export
vfrequency <- function(ntag, colorsname, colorsf, nspecial = 0, name1 = "Metal",
                       name2 = "EMPTY", location1 = 1, location2 = 2, nspecial1 = 1, nspecial2 = 1) {
    if (nspecial > 2) {
        stop("Maximum of 2 special band allowed")
    }
    colorsname <- as.vector(colorsname)
    if (nspecial == 0) {
        amostra <- sample(x = colorsname, size = ntag, replace = TRUE,
                          prob = colorsf)
    } else {
        amostra <- rep(NA, ntag)
        if (length(location1) == 1) {
            auxpst <- location1
        } else {
            auxpst <- sample(x = location1, size = nspecial1)
        }
        amostra[auxpst] <- name1
        if (nspecial == 2) {
            sobroup <- intersect(which(is.na(amostra)), location2)
            if (length(sobroup) == 1) {
                auxpst <- sobroup
            } else {
                auxpst <- sample(x = sobroup, size = nspecial2)
            }
            amostra[auxpst] <- name2
        }
        para_pintar <- which(is.na(amostra))
        amostra[para_pintar] <- sample(x = colorsname, size = length(para_pintar),
                                       replace = TRUE, prob = colorsf)
    }
    amostra <- as.vector(amostra)
    return(amostra)
}

#' @title Life expectancy tag sequence sample
#' @description Create combinations with variable sample probability.
#' @param ntag Number of tag to be used in each animal.
#' @param colorsname Names/Code of color tags to be sample.
#' @param nspecial Number of special tags/codes, such as metallic, 'EMPTY', or flag (min 0, max 2).
#' @param name1 Name of special tag 1.
#' @param name2 Name of special tag 2.
#' @param location1 Position (or group of positions) to special band 1.
#' @param location2 Position (or group of positions) to special band 2.
#' @param nspecial1 Number of special tag 1 that will be present in all sequences genetated.
#' @param nspecial2 Number of special tag 2 that will be present in all sequences genetated.
#' @param currentyear Current year.
#' @param yearsurvival An estimation of the proportion of animals that survive between years.
#' @param lifespan Combinations older the lifespan will be automatically disregard.
#' @param yearusedcombinations The year in which the combination was used.
#' @param usedcombinations Pre used combinations.
#' @param speed Speed for color frequency adjustment.
#' @param ignorecolor Color to be ignored on lifexp.
#' @return A sequencie of tags
#' @examples
#' data(pre_used) # Data example
#' # Create an object contain the name/code of tag colors
#' tcol<-c('Black','Blue','Brown','Gray','Green','Pink','Purple','Red','White','Yellow')
#' genseq(30, 4,tcol, 'lifexp', pre_used[,1:4], yearusedcombinations=pre_used[,5], 
#' yearsurvival= 0.8, lifespan=5, currentyear=2019)
#' @export
lifexp <- function(ntag, colorsname, nspecial = 0, name1 = "Metal",
                   name2 = "EMPTY", location1 = 1, location2 = 2, nspecial1 = 1, nspecial2 = 1,
                   currentyear = NA, yearsurvival = 1, lifespan = NA, yearusedcombinations,
                   usedcombinations, speed = 1, ignorecolor = NA) {
    if (!length(usedcombinations[, 1]) == length(yearusedcombinations)) {
        stop("Review your databse, all combinations must have the information of the year")
    }
    if (is.na(currentyear)) {
        currentyear <- as.numeric(format(Sys.time(), "%Y"))
        warning(paste("You do not provide the current year, it was assume as",
                      currentyear))
    }
    if (is.na(yearsurvival)) {
        stop("You must provide an estimation of year survival")
    }
    if (nspecial > 2) {
        stop("Maximum of 2 special band allowed")
    }
    colorsname <- as.vector(colorsname)
    if (!is.na(lifespan)) {
        usedcombinations <- subset(usedcombinations[, 1:4], yearusedcombinations >
                                       (currentyear - lifespan))
        yearusedcombinations <- subset(yearusedcombinations, yearusedcombinations >
                                           (currentyear - lifespan))
    }
    matrix_usados <- scy(usedcombinations = usedcombinations, yearusedcombinations = yearusedcombinations)
    matrix_restam <- erc(usedcombinations = usedcombinations, yearusedcombinations = yearusedcombinations,
                         currentyear = currentyear, yearsurvival = yearsurvival)
    if (!is.na(ignorecolor)[1]) {
        aux_remocao = NULL
        for (i in 1:length(colnames(matrix_usados))) {
            for (z in 1:length(ignorecolor)) {
                if (intersect(colnames(matrix_usados), ignorecolor[z]) ==
                    colnames(matrix_usados)[i]) {
                    aux_remocao <- c(aux_remocao, i)
                }
            }
        }
        matrix_restam <- matrix_restam[, -aux_remocao]
    }
    total_restante_nat <- colSums(matrix_restam)
    corection_factor <- total_restante_nat/max(total_restante_nat)
    ambiente_aux <- rep(0, length(colorsname))
    names(ambiente_aux) <- colorsname
    cores_compartilhadas <- intersect(names(ambiente_aux), names(corection_factor))
    for (i in 1:length(cores_compartilhadas)) {
        posicao_correcao <- which(names(corection_factor) == cores_compartilhadas[i])
        posicao_ambiente <- which(names(ambiente_aux) == cores_compartilhadas[i])
        ambiente_aux[posicao_ambiente] <- corection_factor[posicao_correcao]
    }
    total_ambiente <- round((1 - (speed * ambiente_aux)), 5)
    if (nspecial == 0) {
        amostra <- sample(x = names(total_ambiente), size = ntag, prob = total_ambiente)
    } else {
        amostra <- rep(NA, ntag)
        if (length(location1) == 1) {
            auxpst <- location1
        } else {
            auxpst <- sample(x = location1, size = nspecial1)
        }
        amostra[auxpst] <- name1
        if (nspecial == 2) {
            sobroup <- intersect(which(is.na(amostra)), location2)
            if (length(sobroup) == 1) {
                auxpst <- sobroup
            } else {
                auxpst <- sample(x = sobroup, size = nspecial2)
            }
            amostra[auxpst] <- name2
        }
        para_pintar <- which(is.na(amostra))
        amostra[para_pintar] <- sample(x = names(total_ambiente), size = length(para_pintar),
                                       prob = total_ambiente)
    }
    amostra <- as.vector(amostra)
    return(amostra)
}
