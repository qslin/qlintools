library(dplyr)
library(UpSetR)
library(ggplot2)
# install.packages("ComplexUpset", type = "source"))
library(ComplexUpset)
library(openxlsx)
library(VennDiagram)
library(SuperExactTest)
library(ComplexHeatmap)
library(circlize)

overlapGroups <- function (inputList, sort = TRUE) {
  data <- UpSetR::fromList(inputList)
  row.names(data) <- unique(unlist(inputList))
  listInputmat <- data == 1
  listInputunique <- unique(listInputmat)
  grouplist <- list()
  for (i in 1:nrow(listInputunique)) {
    currentRow <- listInputunique[i, ]
    myelements <- which(apply(listInputmat, 1, function(x) all(x == currentRow)))
    attr(myelements, "groups") <- currentRow
    grouplist[[paste(colnames(listInputunique)[currentRow], collapse = "::ovlp::")]] <- myelements
    myelements
  }
  if (sort) grouplist <- grouplist[order(sapply(grouplist, function(x) length(x)), decreasing = TRUE)]
  attr(grouplist, "elements") <- unique(unlist(inputList))
  return(grouplist)
}

extractIntersections <- function(inputList, selectedIntersections, resDir, tableFname, csvOut=FALSE, txtOut=FALSE, return=FALSE) {
  if (missing(resDir))        resDir <- getwd()
  if (!dir.exists(resDir))    dir.create(resDir)
  if (missing(tableFname))    tableFname <- deparse(substitute(inputList))
  
  if (nrow(UpSetR::fromList(inputList)) == 0) {
    print("No intersection. Skip this table.")
  }else{
    li <- overlapGroups(inputList)
    li2 <- purrr::map(li, ~ attr(li, "elements")[.x] )
    if (missing(selectedIntersections)) {
      selectedIntersections <- names(li2)
    }else if (!all(selectedIntersections %in% names(li2))) {
      stop("Selected intersections not found.")
    }
    li2 <- li2[selectedIntersections]
    wb <- createWorkbook()
    for (i in seq_along(li2)) {
      if (csvOut) {
        if (!dir.exists(paste0(resDir, "/", tableFname))) dir.create(paste0(resDir, "/", tableFname), recursive = T)
        write.csv(li2[[i]], file = paste0(resDir, "/", tableFname, "/", gsub("\\W+", "_", names(li2)[i]), ".csv"))
      }
      if (txtOut) {
        if (!dir.exists(paste0(resDir, "/", tableFname))) dir.create(paste0(resDir, "/", tableFname), recursive = T)
        write.table(li2[[i]], file = paste0(resDir, "/", tableFname, "/", gsub("\\W+", "_", names(li2)[i]), ".txt"), quote = FALSE, sep = '\t', row.names = FALSE)
      }
      sheet_data <- data.frame(li2[[i]])
      colnames(sheet_data) <- names(li2)[i]
      addWorksheet(wb, i)
      writeData(wb, i, sheet_data)
    }
    saveWorkbook(wb, paste0(resDir, "/", tableFname, ".xlsx"), overwrite = T)
  }
  if (return) return(li2)
}

plotUpset <- function(plotInputList, selectedSets, resDir, upsetFname){
  if (missing(selectedSets))  selectedSets <- names(plotInputList)
  if (missing(resDir))        resDir <- getwd()
  if (!dir.exists(resDir))    dir.create(resDir)
  if (missing(upsetFname))    upsetFname <- deparse(substitute(plotInputList))
  
  if (nrow(UpSetR::fromList(plotInputList)) == 0) {
    print("No intersection. Skip this plot.")
  } else {
    myUpsetPlot <- upset(
      UpSetR::fromList(plotInputList),
      selectedSets,
      keep_empty_groups = TRUE,
      width_ratio = 0.4,
      wrap = TRUE,
      sort_sets = FALSE,
      group_by = 'degree',
      set_sizes = (
        upset_set_size() +
          geom_text(aes(label = after_stat(count)), hjust = 1.1, stat = 'count') +
          expand_limits(y = 2 + max(unlist(lapply(plotInputList, length))) * 1.5) +
          theme(axis.text.x = element_text(angle = 90))
      )
    ) + ggtitle(upsetFname)
    ggsave(
      paste0(resDir, "/", upsetFname, ".png"),
      myUpsetPlot,
      width = 8 + length(names(overlapGroups(plotInputList))) * 1.5,
      height = 16,
      units = "cm",
      limitsize = FALSE
    )
  }
}

intersection_analysis <- function(geneList, selectedSets, resDir, resPrefix, geneListOut=NULL, return=FALSE){
  plotUpset(geneList, selectedSets, resDir, resPrefix)
  if (return) {
    if (geneListOut == "txt") {
      intersections <- extractIntersections(geneList, resDir, resPrefix, csvOut=FALSE, txtOut = TRUE, return=TRUE)
    }else if (geneListOut == "csv") {
      intersections <- extractIntersections(geneList, resDir, resPrefix, csvOut=TRUE, txtOut = FALSE, return=TRUE)
    }else{
      intersections <- extractIntersections(geneList, resDir, resPrefix, csvOut=FALSE, txtOut = FALSE, return=TRUE)
    }
    return(intersections)
  }else{
    if (geneListOut == "txt") {
      intersections <- extractIntersections(geneList, resDir, resPrefix, csvOut=FALSE, txtOut = TRUE, return=FALSE)
    }else if (geneListOut == "csv") {
      intersections <- extractIntersections(geneList, resDir, resPrefix, csvOut=TRUE, txtOut = FALSE, return=FALSE)
    }else{
      intersections <- extractIntersections(geneList, resDir, resPrefix, csvOut=FALSE, txtOut = FALSE, return=FALSE)
    }
  }
}