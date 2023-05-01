xlsx_build <- function(data, file) {
  library(openxlsx)
  # SET UP  -----------------------------------------------------------------
  # to assign classes to columns
  f1 <- function(x){
    class(x) <- 'comma'
    return(x)
  }
  f2 <- function(x){
    class(x) <- 'percentage'
    return(x)
  }
  # audiences and column starts for each
  audiences <- data$definitions$Definition |> unname()
  #
  colstart <- seq(2, 7*(length(audiences)+1), 5)
  psy_colstart <- seq(4, 7*(length(audiences)+1), 5)
  seg_colstart <- seq(4, 7*(length(audiences)+1), 3)
  sum_colstart <- seq(3,2+length(audiences))
  #
  indices <- seq(6, 5*length(audiences)+1, 5)
  psy_indices <- seq(8, (8+5*length(audiences)+1), 5)
  seg_indices <- seq(6, (6+3*length(audiences)+1), 3)
  sum_indices <- seq(3,2+length(audiences))
  
  
  # FORMAT DATA -------------------------------------------------------------
  demos <- data$demos |>
    dplyr::select(-contains("base")) |>
    dplyr::mutate(answer = stringr::str_replace_all(answer, "\\<b\\>|\\</b\\>|\\<br\\>", ""),
                  dplyr::across(dplyr::matches("sample|weighted"), f1),
                  dplyr::across(dplyr::matches("vertical|horizontal"), f2)) |>
    dplyr::rename_with(~paste(., "%"), dplyr::matches("vertical|horizontal"))
  colnames(demos) <- stringr::str_to_title(gsub(".*\\_", "", colnames(demos)))
  
  psychographics <- data$full_dat |>
    dplyr::select(-category_tier_1) |>
    dplyr::mutate(category = stringr::str_replace_all(category, "\\<b\\>|\\</b\\>", ""),
                  question = stringr::str_replace_all(question, "\\<br\\>", " "),
                  dplyr::across(dplyr::matches("sample|weighted"), f1),
                  dplyr::across(dplyr::matches("vertical|horizontal"), f2)) |>
    dplyr::rename_with(~paste(., "%"), dplyr::matches("vertical|horizontal"))
  colnames(psychographics) <- stringr::str_to_title(gsub(".*\\_", "", colnames(psychographics)))
  
  segments <- data$dat |>
    dplyr::mutate(segment = stringr::str_replace_all(segment, "\\<b\\>|\\</b\\>", ""),
                  question = stringr::str_replace_all(question, "\\<br\\>", " "),
                  dplyr::across(dplyr::matches("sample|weighted"), f1),
                  dplyr::across(dplyr::matches("vertical|horizontal"), f2)) |>
    dplyr::rename_with(~paste(., "%"), dplyr::matches("vertical|horizontal"))
  colnames(segments) <- stringr::str_to_title(gsub(".*\\_", "", colnames(segments)))
  
  summary <- data$agree_matrix |>
    dplyr::select(-matches('under')) |>
    dplyr::mutate(segment = stringr::str_replace_all(segment, "\\<b\\>|\\</b\\>", ""),
                  dplyr::across(matches("count"), f1),
                  dplyr::across(matches("over"), f2))
  colnames(summary) <- stringr::str_to_title(gsub(".*\\_", "", colnames(summary)))
  
  # where are the percent columns in each sheet
  demo_pcts <- which(stringr::str_detect(colnames(demos), 'Vert|Horiz'))
  psy_pcts <- which(stringr::str_detect(colnames(psychographics), 'Vert|Horiz'))
  seg_pcts <- which(stringr::str_detect(colnames(segments), 'Vert|Horiz'))
  sum_pcts <- which(stringr::str_detect(colnames(summary), 'Over'))
  
  
  #
  wb <- createWorkbook()
  addWorksheet(wb, "Demos")
  addWorksheet(wb, "Psychographics")
  addWorksheet(wb, "Segments")
  addWorksheet(wb, "Segment Summary")
  # set column header style
  forColHdr <- createStyle(fontColour = "#000000", fontSize = 10, textDecoration = "bold",
                           border = NULL, fgFill = "#DEDEDE",
                           halign = "center", wrapText = TRUE,
                           valign = "center")
  negStyle <- createStyle(fontColour = "#9C0006", bgFill = '#FEF7F7', textDecoration = "bold",
                          border = "left", borderColour = "lightgray")
  posStyle <- createStyle(fontColour = "#006100", bgFill = "#F6FEF6", textDecoration = "bold",
                          border = "left", borderColour = "lightgray")
  
  # adding headers with audience names and merging cells together
  for (i in 1:length(audiences)) {
    # Demos
    writeData(wb,"Demos", audiences[i], startCol = colstart[i], startRow = 1,colNames = TRUE, rowNames = FALSE,
              borders = "none")
    mergeCells(wb, "Demos", cols=colstart[i]:(colstart[(i+1)]-1), rows=1)
    conditionalFormatting(wb, "Demos",
                          cols = indices[i],
                          rows = 3:nrow(demos), rule = "<=90", style = negStyle
    )
    conditionalFormatting(wb, "Demos",
                          cols = indices[i],
                          rows = 3:nrow(demos), rule = ">=110", style = posStyle
    )
    # Psychographics
    writeData(wb,"Psychographics", audiences[i], startCol = psy_colstart[i], startRow = 1,colNames = TRUE, rowNames = FALSE,
              borders = "none")
    mergeCells(wb, "Psychographics", cols=psy_colstart[i]:(psy_colstart[(i+1)]-1), rows=1)
    conditionalFormatting(wb, "Psychographics",
                          cols = psy_indices[i],
                          rows = 3:nrow(psychographics), type = 'between', rule = c(1,90), style = negStyle
    )
    conditionalFormatting(wb, "Psychographics",
                          cols = psy_indices[i],
                          rows = 3:nrow(psychographics), rule = ">=110", style = posStyle
    )
    # Segments
    writeData(wb,"Segments", audiences[i], startCol = seg_colstart[i], startRow = 1,colNames = TRUE, rowNames = FALSE,
              borders = "none")
    mergeCells(wb, "Segments", cols=seg_colstart[i]:(seg_colstart[(i+1)]-1), rows=1)
    conditionalFormatting(wb, "Segments",
                          cols = seg_indices[i],
                          rows = 3:nrow(segments), type = 'between', rule = c(1,90), style = negStyle
    )
    conditionalFormatting(wb, "Segments",
                          cols = seg_indices[i],
                          rows = 3:nrow(segments), rule = ">=110", style = posStyle
    )
    # Summary
    writeData(wb,"Segment Summary", audiences[i], startCol = sum_colstart[i], startRow = 1,colNames = TRUE, rowNames = FALSE,
              borders = "none")
    mergeCells(wb, "Segment Summary", cols=sum_colstart[i], rows=1)
    conditionalFormatting(wb, "Segment Summary",
                          cols = sum_indices[i],
                          rows = 3:nrow(summary), rule = ">=.4", style = posStyle
    )
  }
  
  # adding data to the header
  writeData(wb, "Demos", demos, startCol = 1,startRow = 2,colNames = TRUE,rowNames = FALSE,
            headerStyle = forColHdr, borders='rows',borderStyle = "thin",
            borderColour = '#4F81BD', withFilter = FALSE)
  writeData(wb, "Psychographics", psychographics, startCol = 1,startRow = 2,colNames = TRUE,rowNames = FALSE,
            headerStyle = forColHdr, borders='rows',borderStyle = "thin",
            borderColour = '#4F81BD', withFilter = TRUE)
  writeData(wb, "Segments", segments, startCol = 1,startRow = 2,colNames = TRUE,rowNames = FALSE,
            headerStyle = forColHdr, borders='rows',borderStyle = "thin",
            borderColour = '#4F81BD', withFilter = TRUE)
  writeData(wb, "Segment Summary", summary, startCol = 1,startRow = 2,colNames = TRUE,rowNames = FALSE,
            headerStyle = forColHdr, borders='rows',borderStyle = "thin",
            borderColour = '#4F81BD', withFilter = FALSE)
  
  # styling the header by centering
  centerStyle <- createStyle(halign = "center", textDecoration = "bold",
                             fontSize = 10, fgFill = '#679DD0',
                             border = c("left", "right", "top"), borderColour = '#4F81BD')
  bodyStyle <- createStyle(halign = "center", fontSize = 10)
  tmpx <- "\U2B9f"
  indexStyle <- createStyle(halign = "right", border = c("right"), borderColour = '#4F81BD')
  oneColStyle <- createStyle(halign = "left", fontSize = 10, textDecoration = "bold")
  pctStyle <- createStyle(numFmt = "0.0%")
  addStyle(wb, "Demos", centerStyle, rows = 1, cols = 1:ncol(demos), stack = TRUE)
  addStyle(wb, "Demos", bodyStyle, rows = 1:(nrow(demos)+2), cols = 1:ncol(demos), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Demos", oneColStyle, rows = 1:(nrow(demos)+2), cols = 1, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Demos", indexStyle, rows = 3:(nrow(demos)+2), cols = indices, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Demos", pctStyle, rows = 3:(nrow(demos)+2), cols = demo_pcts, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Psychographics", centerStyle, rows = 1, cols = 1:ncol(psychographics), stack = TRUE)
  addStyle(wb, "Psychographics", bodyStyle, rows = 1:(nrow(psychographics)+2), cols = 1:ncol(psychographics), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Psychographics", oneColStyle, rows = 1:(nrow(psychographics)+2), cols = 1:3, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Psychographics", indexStyle, rows = 1:(nrow(psychographics)+2), cols = psy_indices[1:length(psy_indices)-1], gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Psychographics", pctStyle, rows = 1:(nrow(psychographics)+2), cols = psy_pcts, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Segments", centerStyle, rows = 1, cols = 1:ncol(segments), stack = TRUE)
  addStyle(wb, "Segments", bodyStyle, rows = 1:(nrow(segments)+2), cols = 1:ncol(segments), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Segments", oneColStyle, rows = 1:(nrow(segments)+2), cols = 1:3, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Segments", indexStyle, rows = 3:(nrow(segments)+2), cols = seg_indices[1:length(seg_indices)-1], gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Segments", pctStyle, rows = 3:(nrow(segments)+2), cols = seg_pcts, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Segment Summary", centerStyle, rows = 1, cols = 1:ncol(summary), stack = TRUE)
  addStyle(wb, "Segment Summary", bodyStyle, rows = 1:(nrow(summary)+2), cols = 1:ncol(summary), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Segment Summary", oneColStyle, rows = 1:(nrow(summary)+2), cols = 1:1, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, "Segment Summary", pctStyle, rows = 3:(nrow(summary)+2), cols = sum_pcts, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, "Demos", cols = 1, widths = "auto")
  setColWidths(wb, "Psychographics", cols = c(1, 2), widths = c(25, 55))
  setColWidths(wb, "Segments", cols = c(1, 2), widths = c(25, 55))
  setColWidths(wb, "Segment Summary", cols = c(1,2,3,4,5), widths = c(25, 5, 20,20,20))
  saveWorkbook(wb, file, overwrite = TRUE)
}
