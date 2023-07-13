# Helper functions for Simmons App # 

# Read in Simmons Datahaul; clean; get group names and totals; pull demos
read_simmons <- function(loc) {
  keepers <- c("Base: Study Universe", "Demographics", "Political Outlook/Affiliation & Voting",
               "Hispanic Demos/Attitudes & HH Language", "Psychographics")
  message("Reading data...")
  dat <- openxlsx::read.xlsx(loc, startRow = 1)
  # pull simmons metadata
  meta <- clean_meta(colnames(dat)[1])
  # read in group definitions from first row
  grps <- dat[1,]
  # pull out the audience definitions
  grp_locations <- seq(6, ncol(grps), by = 5)[-1]
  grp <- paste0("g", seq_len(length(grp_locations))+1)
  grp_definitions <- dplyr::tibble(nm = trimws(unlist(grps[1, grp_locations]))) |>
    dplyr::pull(nm)
  grp_def <- dplyr::tibble("Group" = grp,
                           "Definition" = grp_definitions) |> 
    dplyr::mutate(Definition = trimws(stringr::str_replace_all(Definition, "xml:space=\"preserve\">", "")),
                  Definition = stringr::str_replace_all(Definition, "&amp;", "&"),
                  Definition = stringr::str_replace_all(Definition, "\\{|\\}", ""))
  # drop group names; reset colnames; clean
  dat <- dat[-1,] |>
    janitor::row_to_names(row_number = 1) |>
    janitor::clean_names()
  # rename the data
  cat_names <- names(dat)[1:5]
  groups <- rep(seq_len((ncol(dat)/5) - 1), each = 5)
  name_parts <- c('sample', 'weighted', 'vertical', 'horizontal', 'index')
  new_names <- c(cat_names, paste0("g", groups, "_", name_parts))
  # rename with new names
  dat <- dat |>
    purrr::set_names(nm = new_names) |>
    dplyr::rename_at(dplyr::vars(contains("g1")), ~(sub('g1', 'base', .)))
  # drop unused categories
  dat <- dat |>
    dplyr::filter(category_tier_1 %in% keepers) |> 
    dplyr::mutate(across(matches("sample|weighted|vertical|horizontal|index"), as.numeric)) 
  # demographics function
  demos <- build_demographics(dat)
  gc()
  list("dat" = dat, "meta" = meta, "grp_def" = grp_def, "demos" = demos)
}

# pull out the relevant demographics for each Datahaul group
build_demographics <- function(tbl) {
  message("Pulling demographics...")
  total <- tbl |>
    dplyr::filter(category_tier_1 == "Base: Study Universe") |>
    dplyr::select(-c(category_tier_1:question)) |> 
    dplyr::mutate(answer = "<b>Total Universe</b>")
  gender <- tbl |> 
    dplyr::filter(question == "Bases" & answer %in% c("Men", "Women")) |>
    dplyr::select(-c(category_tier_1:question)) |> 
    dplyr::mutate(answer = glue::glue("<b>Gender:</b> {answer}"))
  age <- tbl |> 
    dplyr::filter(category_tier_2 == "Respondent" & answer %in% c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")) |>
    dplyr::select(-c(category_tier_1:question)) |>
    dplyr::mutate(answer = glue::glue("<b>Age:</b> {answer}"))
  generation <- tbl |> 
    dplyr::filter(category_tier_2 == "Respondent" & question == "Generations") |>
    dplyr::filter(!stringr::str_detect(answer, "Early|Late|Pre-")) |>
    dplyr::select(-c(category_tier_1:question)) |>
    dplyr::mutate(answer = gsub("\\[.*", "", answer),
                  answer = glue::glue("<b>Generation:</b><br> {answer}"))
  educ <- tbl |> 
    dplyr::filter(category_tier_2 == "Respondent" & question == "Highest Degree Received") |>
    dplyr::select(-c(category_tier_1:question)) |>
    dplyr::mutate(answer = glue::glue("<b>Highest Degree:</b><br> {answer}"))
  pid <- tbl |> 
    dplyr::filter(question == "Political Affiliation") |>
    dplyr::select(-c(category_tier_1:question)) |>
    dplyr::mutate(answer = glue::glue("<b>PID:</b> {answer}"))
  pol <- tbl |> 
    dplyr::filter(question == "Political Outlook") |>
    dplyr::select(-c(category_tier_1:question)) |>
    dplyr::mutate(answer = glue::glue("<b>Political Outlook:</b><br> {answer}"))
  hhi <- tbl |> 
    dplyr::filter(category_tier_2 == "Household" & question == "Household Income {HH}") |>
    dplyr::select(-c(category_tier_1:question)) |>
    dplyr::mutate(answer = stringr::str_replace_all(answer, "-", "-$"),
                  answer = glue::glue("<b>Household Income:</b><br> ${answer}"),
                  answer = stringr::str_replace_all(answer, "Dollars", ""))
  region <- tbl |> 
    dplyr::filter(category_tier_2 == "Household" & question == "Marketing Region {HH}") |>
    dplyr::select(-c(category_tier_1:question)) |>
    dplyr::mutate(answer = glue::glue("<b>Marketing Region:</b><br> {answer}"))
  county <- tbl |> 
    dplyr::filter(category_tier_2 == "Household" & question == "County Size {HH}") |>
    dplyr::select(-c(category_tier_1:question)) |>
    dplyr::mutate(answer = glue::glue("<b>County Size:</b> {answer}"))
  race <- tbl |> 
    dplyr::filter(category_tier_2 == "Respondent" & question == "Race") |>
    dplyr::select(-c(category_tier_1:question)) |>
    dplyr::mutate(answer = glue::glue("<b>Race:</b> {answer}"))
  ethnicity <- tbl |> 
    dplyr::filter(category_tier_1 == "Hispanic Demos/Attitudes & HH Language" & 
                    question == "Spanish Or Hispanic Origin") |>
    dplyr::select(-c(category_tier_1:question)) |>
    dplyr::mutate(answer = glue::glue("<b>Spanish or Hispanic Origin:</b> {answer}"))
  
  tmpout <- rbind(total, gender, age, generation, educ, pid, pol, hhi,
                  region, county, race, ethnicity)
}

# cleaning meta data for initial read in and check 
clean_meta <- function(data){
  data <- trimws(stringr::str_replace_all(data, "xml:space=\"preserve\">", ""))
  data <- stringr::str_replace(data, "Title:", "<b>Title:</b>")
  data <- stringr::str_replace(data, ".Study:", "<br/><b>Study:</b>")
  data <- stringr::str_replace(data, ".Weight.Type:", "<br/><b>Weight.Type:</b>")
  data <- stringr::str_replace(data, ".Start.Field.Date", "<br/><b>Start.Field.Date:</b>")
  data <- stringr::str_replace(data, ".End.Field.Date:", "<br/><b>End.Field.Date:</b>")
  data <- stringr::str_replace(data, ".Run.Date:", "<br/><b>Run.Date:</b>")
  data <- gsub("\\*.*", "", data)
  data <- stringr::str_replace_all(data, "\\.\\.", "\\.")
  data <- stringr::str_replace_all(data, "\\.", " ")
  return(data)
}

# function to build demographic table in DT
build_demo_tbl <- function(data) {
  audiences <- data$grp_def$Definition |> unname()
  demo_data <- data$demos |> 
    dplyr::select(-dplyr::matches("base|horizontal"))
  # 1. build the container
  header.style <- "th {position: sticky; color: white; background-color: #2D3D4F;}"
  sketch <- htmltools::withTags(table(
    class = 'compact',
    style(type = "text/css", header.style),
    thead(
      tr(
        th(rowspan = 2, "Demographic"),
        lapply(audiences, th, colspan = 4,
               style = "text-align: center;
               border-bottom-width: 1px;
               border-bottom-style: solid; border-bottom-color: white")
      ),
      tr(
        lapply(rep(c('Sample', "Weighted", 'Vertical', "Index"), length(audiences)), th)
      )
    )
  ))
  # 2. build the table
  DT::datatable(demo_data, 
                container = sketch,
                escape = FALSE,
                # extensions = c('FixedHeader'),
                rownames = FALSE,
                class = c("nowrap", "stripe", "hover"),
                options = list(
                  # fixedHeader = T,
                  dom = 'lftrip',
                  columnDefs = list(
                    list(className = 'dt-center', targets = "_all")
                  ),
                  pageLength = 65,
                  lengthChange = TRUE),
                caption = glue::glue("Audience Demographics")) |> 
    DT::formatStyle(columns = c('answer'), `text-align` = 'left') |> 
    DT::formatPercentage(columns = stringr::str_detect(names(demo_data), "vert"), 1) |>
    DT::formatCurrency(stringr::str_detect(names(demo_data), 'sample|weighted'),
                       currency = "",  mark = ",", digits = 0) |> 
    DT::formatStyle(
      stringr::str_detect(names(demo_data), "index"),
      color = DT::styleInterval(c(91, 109), c('red', '', '#1D741B')),
      fontWeight = DT::styleInterval(c(91, 109), c('bold', '', 'bold')),
    ) |>
    DT::formatStyle(columns = colnames(demo_data), fontSize = '85%')
}


# segment psychographics; filter vertical and index for each group; merge back
psychographic_segments <- function(data, intensity, vertical, index1, index2, compare, empty) {
  
  # load Simmons psychographic statements
  segments <- readRDS(here::here("data", "all_simmons_segments.rds"))
  # list of segments
  segment_list <- names(segments)
  # function for rowwise comparison 
  is.ou <- function(x) (x > index1 & index2 > x)
  
  # function: pull relevant segment statements from datahaul
  list_index <- function(data, cat) {
    data |>
      dplyr::filter(question %in% segments[[cat]]) |>
      dplyr::select(question,
                    answer,
                    dplyr::matches("^g[1-9]_sample|^g[1-9]_index|^g[1-9]_vertical"))
  }
  
  # pull all matching psychographic statements from segments
  statements <- segment_list |>
    purrr::map(list_index, data = data$dat) |>
    purrr::set_names(segment_list) |>
    data.table::rbindlist(idcol = 'segment') |>
    tibble::tibble() |>
    dplyr::mutate(across(matches("sample|vertical|index"), as.numeric))
  
  # function: filter each audience by vertical and index
  filter_index <- function(group, intensity, vertical, index1, index2, compare) {
    quo_groupindex <- dplyr::sym(glue::glue("{group}_index"))
    quo_groupvert <- dplyr::sym(glue::glue("{group}_vertical"))
    
    # keep for the eventual merge
    all_statements <- statements |>
      dplyr::select(segment:answer) |>
      dplyr::filter(answer == intensity)
    
    if (compare == TRUE) {
      # keep statements meeting vertical
      tmp <- statements |>
        dplyr::select(c(segment:answer, dplyr::matches(group))) |>
        dplyr::filter(
          answer == intensity &
            !!quo_groupvert >= vertical
          # (!!quo_groupindex <= index1 |
          #    !!quo_groupindex >= index2)
        )
    } else {
      # keep statements meeting vertical and index
      tmp <- statements |>
        dplyr::select(c(segment:answer, dplyr::matches(group))) |>
        dplyr::filter(
          answer == intensity &
            !!quo_groupvert >= vertical &
            (!!quo_groupindex <= index1 |
               !!quo_groupindex >= index2)
        )
    }
    
    # full join so we have all statements
    tmp <- dplyr::full_join(all_statements, tmp) |> 
      dplyr::mutate(segment = stringr::str_to_title(stringr::str_replace_all(segment, "_", " ")),
                    segment = paste0("<b>", segment, "</b>"),
                    question = sapply(question, function(x) paste(strwrap(x, 70), collapse = "<br>")))
  }
  
  # filter all psychographics for tab 4
  filter_full_index <- function(group, intensity, vertical, index1, index2, compare){
    quo_groupindex <- dplyr::sym(glue::glue("{group}_index"))
    quo_groupvert <- dplyr::sym(glue::glue("{group}_vertical"))
    
    # keep all pscyhographic statments matching intensity
    all_psychographics <- data$dat |>
      dplyr::select(category_tier_1, category = category_tier_2, question, answer) |>
      dplyr::filter(answer == intensity)
    
    if (compare == TRUE) {
      # keep statements matching vertical
      tmp <- data$dat |>
        dplyr::select(c(category_tier_1, category = category_tier_2, question,
                        answer, dplyr::matches(group))) |>
        dplyr::filter(
          answer == intensity &
            !!quo_groupvert >= vertical
          # (!!quo_groupindex <= index1 |
          #    !!quo_groupindex >= index2)
        )
    } else {
      # keep statements matching vertical and index
      tmp <- data$dat |>
        dplyr::select(c(category_tier_1, category = category_tier_2, question,
                        answer, dplyr::matches(group))) |>
        dplyr::filter(
          answer == intensity &
            !!quo_groupvert >= vertical &
            (!!quo_groupindex <= index1 |
               !!quo_groupindex >= index2)
        )
    }
    
    # # full join with all statements 
    tmp <- dplyr::full_join(all_psychographics, tmp) |>
      dplyr::mutate(category = paste0("<b>", category, "</b>"),
                    question = sapply(question, function(x) paste(strwrap(x, 70), collapse = "<br>")))
  }
  
  agreement_count <- function(group, intensity, vertical, index1, index2){
    segtest <- c('segment', "FALSE", "TRUE")
    quo_groupindex <- dplyr::sym(glue::glue("{group}_index"))
    quo_groupvert <- dplyr::sym(glue::glue("{group}_vertical"))
    
    neg_out <- statements |>
      dplyr::select(c(segment:answer, dplyr::matches(group))) |>
      dplyr::filter(answer == intensity) |> 
      dplyr::count(segment, response = (!!quo_groupvert >= vertical & !!quo_groupindex <= index1)) |> 
      tidyr::pivot_wider(id_cols = segment,
                         names_from = response,
                         values_from = n)
    
    if (ncol(neg_out) < 3) {
      miss <- segtest[-match(colnames(neg_out), segtest)]
      neg_out <- neg_out |> 
        dplyr::mutate({{miss}} := 0) |> 
        purrr::set_names(nm = c("segment", "insig", "sig")) |>
        dplyr::mutate(dplyr::across(insig:sig, ~ifelse(is.na(.), 0, .)),
                      count = sig+insig,
                      neg_frac = sig/(sig+insig)) |>
        dplyr::select(segment,count, neg_frac) |>
        purrr::set_names(nm = c("segment", "count", glue::glue("{group}_under-index")))
    } else {
      neg_out <- neg_out |> 
        purrr::set_names(nm = c("segment", "insig", "sig")) |>
        dplyr::mutate(dplyr::across(insig:sig, ~ifelse(is.na(.), 0, .)),
                      count = sig+insig,
                      neg_frac = sig/(sig+insig)) |>
        dplyr::select(segment,count, neg_frac) |>
        purrr::set_names(nm = c("segment", "count", glue::glue("{group}_under-index")))
    }
    
    
    pos_out <- statements |>
      dplyr::select(c(segment:answer, dplyr::matches(group))) |>
      dplyr::filter(answer == intensity) |> 
      dplyr::count(segment, response = (!!quo_groupvert >= vertical & !!quo_groupindex >= index2)) |>
      tidyr::pivot_wider(id_cols = segment,
                         names_from = response,
                         values_from = n)
    
    if (ncol(pos_out) < 3) {
      miss <- segtest[-match(colnames(pos_out), segtest)]
      pos_out <- pos_out |> 
        dplyr::mutate({{miss}} := 0) |> 
        purrr::set_names(nm = c("segment", "insig", "sig")) |> 
        dplyr::mutate(dplyr::across(insig:sig, ~ifelse(is.na(.), 0, .)),
                      pos_frac = sig/(sig+insig)) |> 
        dplyr::select(segment, pos_frac) |> 
        purrr::set_names(nm = c("segment", glue::glue("{group}_over-index")))
    } else {
      pos_out <- pos_out |> 
        purrr::set_names(nm = c("segment", "insig", "sig")) |> 
        dplyr::mutate(dplyr::across(insig:sig, ~ifelse(is.na(.), 0, .)),
                      pos_frac = sig/(sig+insig)) |> 
        dplyr::select(segment, pos_frac) |> 
        purrr::set_names(nm = c("segment", glue::glue("{group}_over-index")))
    }
    
    tmp <- dplyr::left_join(neg_out, pos_out) |> 
      dplyr::mutate(segment = stringr::str_to_title(stringr::str_replace_all(segment, "_", " ")),
                    segment = paste0("<b>", segment, "</b>"))
    return(tmp)
  }
  
  # create tibble to pmap across
  tmp_tbl <- tidyr::crossing(data$grp_def,
                             intensity = intensity,
                             vertical = vertical/100,
                             index1 = index1,
                             index2 = index2,
                             compare = compare)
  
  # mapping with filter_index
  dat <- purrr::pmap(
    list(
      tmp_tbl$Group,
      tmp_tbl$intensity,
      tmp_tbl$vertical,
      tmp_tbl$index1,
      tmp_tbl$index2,
      tmp_tbl$compare
    ),
    filter_index
  ) |>
    purrr::set_names(nm = tmp_tbl$Definition) |>
    purrr::reduce(dplyr::full_join)
  
  # mapping with filter full index
  full_dat <- purrr::pmap(
    list(
      tmp_tbl$Group,
      tmp_tbl$intensity,
      tmp_tbl$vertical,
      tmp_tbl$index1,
      tmp_tbl$index2,
      tmp_tbl$compare
    ),
    filter_full_index
  ) |>
    purrr::set_names(nm = tmp_tbl$Definition) |>
    purrr::reduce(dplyr::full_join)
  
  agree_matrix <- purrr::pmap(
    list(
      tmp_tbl$Group,
      tmp_tbl$intensity,
      tmp_tbl$vertical,
      tmp_tbl$index1,
      tmp_tbl$index2
    ),
    agreement_count
  ) |>
    purrr::set_names(nm = tmp_tbl$Definition) |>
    purrr::reduce(dplyr::full_join)
  
  # compare = to keep all column data when one index in the row exceeds thresholds
  if (compare == TRUE) {
    dat <- dat |>
      dplyr::rowwise() |>
      dplyr::mutate(exclude = all(dplyr::c_across(dplyr::matches("index")) |> is.ou())) |>
      dplyr::mutate(dplyr::across(dplyr::matches("sample|vert|index"), ~ifelse(exclude == TRUE, NA, .))) |> 
      dplyr::select(-exclude)
    
    full_dat <- full_dat |>
      dplyr::rowwise() |>
      dplyr::mutate(exclude = all(dplyr::c_across(dplyr::matches("index")) |> is.ou())) |>
      dplyr::mutate(dplyr::across(dplyr::matches("sample|weighted|vert|horiz|index"), ~ifelse(exclude == TRUE, NA, .))) |> 
      dplyr::select(-exclude)
  } else {
    dat
    full_dat
  }
  
  if (empty == TRUE) {
    dat <- dat |> dplyr::filter(dplyr::if_any(dplyr::matches("sample|vert|index"), ~ !is.na(.)))
    full_dat <- full_dat |> dplyr::filter(dplyr::if_any(dplyr::matches("sample|vert|index"), ~ !is.na(.)))
  } else {
    dat
    full_dat
  }
  list(dat = dat, full_dat = full_dat, agree_matrix = agree_matrix, 
       demos = data$demos, definitions = data$grp_def)
}

# DT datatable for agreement summary  
build_agreement_tbl <- function(data) {
  audiences <- data$definitions$Definition |> unname()
  df <- data$agree_matrix |> 
    dplyr::select(-matches("under"))
  # 1. build the container
  header.style <- "th {position: sticky; color: white; background-color: #2D3D4F;}"
  sketch <- htmltools::withTags(table(
    class = 'compact',
    style(type = "text/css", header.style),
    thead(
      tr(
        th(rowspan = 2, "Segment"),
        th(rowspan = 2, "# Statements"),
        lapply(audiences, th, colspan = 1, 
               style = "text-align: center;
               border-bottom-width: 1px;
               border-bottom-style: solid; border-bottom-color: white")
      ),
      tr(
        lapply(rep(c('Over-Index %'), length(audiences)), th)
      )
    )
  ))
  # 2. build the table 
  DT::datatable(df,
                container = sketch,
                escape = FALSE,
                rownames = FALSE,
                class = c("nowrap", "stripe", "hover"),
                options = list(
                  dom = 'lftrip',
                  columnDefs = list(
                    list(className = 'dt-center', targets = "_all")
                    # list(visible = FALSE, targets = "answer")
                  ),
                  pageLength = 35,
                  lengthChange = TRUE),
                caption = glue::glue("Segment Summary: {unique(data$full_dat$answer)}")) |> 
    DT::formatStyle(columns = c('segment'), `text-align` = 'left') |> 
    DT::formatPercentage(columns = stringr::str_detect(names(df), "over"), 1) |>
    DT::formatStyle(
      stringr::str_detect(names(df), "over"),
      fontWeight = DT::styleInterval(c(.399), c('', 'bold'))) |> 
    DT::formatStyle(columns = colnames(df), fontSize = '85%')
}

# DT datatable for all psychographics  
build_psychographic_tbl <- function(data) {
  audiences <- data$definitions$Definition |> unname()
  df <- data$full_dat |> 
    dplyr::select(-matches("tier_1|weighted|horizontal"))
  # 1. build the container
  header.style <- "th {position: sticky; color: white; background-color: #2D3D4F;}"
  sketch <- htmltools::withTags(table(
    class = 'compact',
    style(type = "text/css", header.style),
    thead(
      tr(
        th(rowspan = 2, "Category"),
        th(rowspan = 2, "Question"),
        th(rowspan = 2, "Answer"),
        lapply(audiences, th, colspan = 3, 
               style = "text-align: center;
               border-bottom-width: 1px;
               border-bottom-style: solid; border-bottom-color: white")
      ),
      tr(
        lapply(rep(c('Sample', 'Vertical', "Index"), length(audiences)), th)
      )
    )
  ))
  # 2. build the table 
  DT::datatable(df,
                container = sketch,
                escape = FALSE,
                rownames = FALSE,
                class = c("nowrap", "stripe", "hover"),
                options = list(
                  dom = 'lftrip',
                  columnDefs = list(
                    list(className = 'dt-center', targets = "_all"),
                    list(visible = FALSE, targets = "answer")
                  ),
                  pageLength = 25,
                  lengthChange = TRUE),
                caption = glue::glue("Psychographics statements: {unique(data$full_dat$answer)}")) |> 
    DT::formatStyle(columns = c('category','question'), `text-align` = 'left') |> 
    DT::formatPercentage(columns = stringr::str_detect(names(df), "vert"), 1) |> 
    DT::formatCurrency(stringr::str_detect(names(df), 'sample'),
                       currency = "",  mark = ",", digits = 0) |> 
    DT::formatStyle(
      stringr::str_detect(names(df), "index"),
      color = DT::styleInterval(c(91, 109), c('red', '', '#1D741B')),
      fontWeight = DT::styleInterval(c(91, 109), c('bold', '', 'bold')),
    ) |> 
    DT::formatStyle(columns = colnames(df), fontSize = '85%')
}

# DT datatable for psychographic segments 
build_segment_tbl <- function(data) {
  audiences <- data$definitions$Definition |> unname()
  # 1. build the container
  header.style <- "th {position: sticky; color: white; background-color: #2D3D4F;}"
  sketch <- htmltools::withTags(table(
    class = 'compact',
    style(type = "text/css", header.style),
    thead(
      tr(
        th(rowspan = 2, "Segment"),
        th(rowspan = 2, "Question"),
        th(rowspan = 2, "Answer"),
        lapply(audiences, th, colspan = 3, 
               style = "text-align: center;
               border-bottom-width: 1px;
               border-bottom-style: solid; border-bottom-color: white")
      ),
      tr(
        lapply(rep(c('Sample', 'Vertical', "Index"), length(audiences)), th)
      )
    )
  ))
  # 2. build the table 
  DT::datatable(data$dat, 
                container = sketch,
                escape = FALSE,
                # extensions = c("Buttons"),
                rownames = FALSE,
                class = c("nowrap", "stripe", "hover"),
                options = list(
                  # fixedHeader = T,
                  dom = 'lftrip',
                  # buttons = 'excel',
                  columnDefs = list(
                    list(className = 'dt-center', targets = "_all"),
                    list(visible = FALSE, targets = "answer")
                    # list(width = '200px', targets = 1)
                  ),
                  pageLength = 25,
                  lengthChange = TRUE),
                caption = glue::glue("Psychographics statements: {unique(data$dat$answer)}")) |> 
    DT::formatStyle(columns = c('segment','question'), `text-align` = 'left') |> 
    DT::formatPercentage(stringr::str_detect(names(data$dat), "vert"), 1) |>
    DT::formatCurrency(stringr::str_detect(names(data$dat), 'sample'),
                       currency = "",  mark = ",", digits = 0) |>
    DT::formatStyle(
      stringr::str_detect(names(data$dat), "index"),
      color = DT::styleInterval(c(91, 109), c('red', '', '#1D741B')),
      fontWeight = DT::styleInterval(c(91, 109), c('bold', '', 'bold')),
    ) |> 
    DT::formatStyle(columns = colnames(data$dat), fontSize = '85%')
}

