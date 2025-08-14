# Helper functions for Simmons App #
KEEPERS <- c(
  "Base: Study Universe",
  "Demographics",
  "Political Outlook/Affiliation & Voting",
  "Hispanic Demos/Attitudes & HH Language",
  "Psychographics"
)

# Read in Simmons Datahaul; clean; get group names and totals; pull demos
read_simmons <- function(loc) {
  message("Reading data...")
  # read in the spreadsheet
  raw <- openxlsx::read.xlsx(loc, startRow = 1, colNames = FALSE)

  # locate the header row by text in first column -> Category Tier 1
  first_col <- names(raw)[1]
  header_idx <- which(grepl(
    "Category\\s*\\(\\s*Tier\\s*1\\s*\\)",
    raw[[first_col]],
    ignore.case = TRUE
  ))[1]

  # group definitions live in the row immediately above the header
  grps <- raw[header_idx - 1, , drop = FALSE]

  # metadata = everything above the group row
  meta_rows <- seq_len(max(header_idx - 2, 0))
  meta_text <- if (length(meta_rows)) {
    paste(raw[[first_col]][meta_rows], collapse = ".")
  } else {
    ""
  }
  meta_text <- gsub(" ", ".", meta_text)
  meta <- trimws(clean_meta(meta_text))

  # slice down to the table, promote header, clean names
  dat <- raw[-seq_len(header_idx - 1), , drop = FALSE] |>
    janitor::row_to_names(1) |>
    janitor::clean_names()

  # Expect first 5 are: category_tier_1, category_tier_2, category, question, answer
  if (ncol(dat) < 10) {
    stop("Input has too few columns after header normalization.")
  }

  # pull the audience definitions
  grp_locations <- seq(6, ncol(grps), by = 5)[-1]
  grp_ids <- paste0("g", seq_along(grp_locations) + 1)
  grp_definitions <- trimws(unlist(grps[1, grp_locations]))
  grp_def <- tibble::tibble(
    Group = grp_ids,
    Definition = grp_definitions
  ) |>
    dplyr::mutate(
      Definition = stringr::str_replace_all(
        Definition,
        "xml:space=\"preserve\">",
        ""
      ),
      Definition = stringr::str_replace_all(Definition, "&amp;", "&"),
      Definition = stringr::str_replace_all(Definition, "\\{|\\}|\\(|\\)", ""),
      Definition = trimws(Definition)
    )

  # rename the data columns
  cat_names <- names(dat)[1:5]
  groups <- rep(seq_len((ncol(dat) / 5) - 1), each = 5)
  name_parts <- c("sample", "weighted", "vertical", "horizontal", "index")
  new_names <- c(cat_names, paste0("g", groups, "_", name_parts))
  dat <- dat |>
    purrr::set_names(new_names) |>
    dplyr::rename_with(~ sub("g1", "base", .x), dplyr::contains("g1"))

  # filter categories + convert numerics
  dat <- dat |>
    dplyr::filter(category_tier_1 %in% KEEPERS) |>
    dplyr::mutate(across(
      matches("sample|weighted|vertical|horizontal|index"),
      as.numeric
    ))
  # demographics function
  demos <- build_demographics(dat)
  gc()
  list("dat" = dat, "meta" = meta, "grp_def" = grp_def, "demos" = demos)
}

# pull out the relevant demographics for each Datahaul group
build_demographics <- function(tbl) {
  message("Pulling demographics...")
  # tiny helpers used below
  label <- function(df, fmt) dplyr::mutate(df, answer = glue::glue(fmt))
  drop_meta <- function(df) {
    dplyr::select(
      df,
      -category_tier_1,
      -category_tier_2,
      -category_tier_3,
      -question
    )
  }

  # spec: each entry defines how to filter + how to label; optional post() to tweak
  demo_specs <- list(
    total = list(
      filter = ~ dplyr::filter(.x, category_tier_1 == "Base: Study Universe"),
      build = ~ .x |>
        drop_meta() |>
        dplyr::mutate(answer = "<b>Total Universe</b>")
    ),
    gender = list(
      filter = ~ dplyr::filter(
        .x,
        question == "Bases",
        answer %in% c("Men", "Women")
      ),
      build = ~ .x |> drop_meta() |> label("<b>Gender:</b> {answer}")
    ),
    age = list(
      filter = ~ dplyr::filter(
        .x,
        category_tier_2 == "Respondent",
        answer %in% c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
      ),
      build = ~ .x |> drop_meta() |> label("<b>Age:</b> {answer}")
    ),
    generation = list(
      filter = ~ dplyr::filter(
        .x,
        category_tier_2 == "Respondent",
        question == "Generations"
      ) |>
        dplyr::filter(!stringr::str_detect(answer, "Early|Late|Pre-")),
      build = ~ .x |>
        drop_meta() |>
        dplyr::mutate(
          answer = gsub("\\[.*", "", answer),
          answer = trimws(answer)
        ) |>
        label("<b>Generation:</b><br> {answer}")
    ),
    marital = list(
      filter = ~ dplyr::filter(
        .x,
        category_tier_2 == "Respondent",
        question == "Marital Status",
        answer %in%
          c(
            "Never Married",
            "Now Married",
            "Widowed",
            "Divorced",
            "Separated (legally)"
          )
      ),
      build = ~ .x |> drop_meta() |> label("<b>Marital Status:</b> {answer}")
    ),
    educ = list(
      filter = ~ dplyr::filter(
        .x,
        category_tier_2 == "Respondent",
        question == "Highest Degree Received"
      ),
      build = ~ .x |>
        drop_meta() |>
        label("<b>Highest Degree:</b><br> {answer}")
    ),
    pid = list(
      filter = ~ dplyr::filter(.x, question == "Political Affiliation"),
      build = ~ .x |> drop_meta() |> label("<b>PID:</b> {answer}")
    ),
    pol = list(
      filter = ~ dplyr::filter(.x, question == "Political Outlook"),
      build = ~ .x |>
        drop_meta() |>
        label("<b>Political Outlook:</b><br> {answer}")
    ),
    hhi = list(
      filter = ~ dplyr::filter(
        .x,
        category_tier_2 == "Household",
        question == "Household Income {HH}"
      ),
      build = ~ .x |>
        drop_meta() |>
        dplyr::mutate(
          answer = stringr::str_replace_all(answer, "[\u2013\u2014]", "-"),
          answer = stringr::str_replace_all(answer, "-", "-$"),
          answer = stringr::str_replace_all(answer, "(?i)\\s*Dollars", "")
        ) |>
        # keep your simple, robust approach: put the leading $ in the label, not via regex groups
        label("<b>Household Income:</b><br> ${answer}")
    ),
    region = list(
      filter = ~ dplyr::filter(
        .x,
        category_tier_2 == "Household",
        question == "Marketing Region {HH}"
      ),
      build = ~ .x |>
        drop_meta() |>
        label("<b>Marketing Region:</b><br> {answer}")
    ),
    county = list(
      filter = ~ dplyr::filter(
        .x,
        category_tier_2 == "Household",
        question == "County Size {HH}"
      ),
      build = ~ .x |> drop_meta() |> label("<b>County Size:</b> {answer}")
    ),
    race = list(
      filter = ~ dplyr::filter(
        .x,
        category_tier_2 == "Respondent",
        question == "Race"
      ),
      build = ~ .x |> drop_meta() |> label("<b>Race:</b> {answer}")
    ),
    ethnicity = list(
      filter = ~ dplyr::filter(
        .x,
        category_tier_1 == "Hispanic Demos/Attitudes & HH Language",
        question == "Spanish Or Hispanic Origin"
      ),
      build = ~ .x |>
        drop_meta() |>
        label("<b>Spanish or Hispanic Origin:</b> {answer}")
    )
  )

  pieces <- purrr::map_dfr(demo_specs, function(spec) {
    f_filter <- rlang::as_function(spec$filter)
    f_build <- rlang::as_function(spec$build)
    f_build(f_filter(tbl))
  })
  return(pieces)
}

# cleaning meta data for initial read in and check
clean_meta <- function(data) {
  data <- trimws(stringr::str_replace_all(data, "xml:space=\"preserve\">", ""))
  data <- stringr::str_replace(data, "Title:", "<b>Title:</b>")
  data <- stringr::str_replace(data, ".Study:", "<br/><b>Study:</b>")
  data <- stringr::str_replace(
    data,
    ".Weight.Type:",
    "<br/><b>Weight.Type:</b>"
  )
  data <- stringr::str_replace(
    data,
    ".Start.Field.Date",
    "<br/><b>Start.Field.Date:</b>"
  )
  data <- stringr::str_replace(
    data,
    ".End.Field.Date:",
    "<br/><b>End.Field.Date:</b>"
  )
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
        lapply(
          audiences,
          th,
          colspan = 4,
          style = "text-align: center;
               border-bottom-width: 1px;
               border-bottom-style: solid; border-bottom-color: white"
        )
      ),
      tr(
        lapply(
          rep(c('Sample', "Weighted", 'Vertical', "Index"), length(audiences)),
          th
        )
      )
    )
  ))
  # 2. build the table
  DT::datatable(
    demo_data,
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
      lengthChange = TRUE
    ),
    caption = glue::glue("Audience Demographics")
  ) |>
    DT::formatStyle(columns = c('answer'), `text-align` = 'left') |>
    DT::formatPercentage(
      columns = stringr::str_detect(names(demo_data), "vert"),
      1
    ) |>
    DT::formatCurrency(
      stringr::str_detect(names(demo_data), 'sample|weighted'),
      currency = "",
      mark = ",",
      digits = 0
    ) |>
    DT::formatStyle(
      stringr::str_detect(names(demo_data), "index"),
      color = DT::styleInterval(c(91, 109), c('red', '', '#1D741B')),
      fontWeight = DT::styleInterval(c(91, 109), c('bold', '', 'bold')),
    ) |>
    DT::formatStyle(columns = colnames(demo_data), fontSize = '85%')
}


# segment psychographics; filter vertical and index for each group; merge back
psychographic_segments <- function(
  data,
  intensity,
  vertical,
  index1,
  index2,
  compare,
  empty
) {
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
      dplyr::select(
        question,
        answer,
        dplyr::matches("^g[1-9]_sample|^g[1-9]_index|^g[1-9]_vertical")
      )
  }

  # pull all matching psychographic statements from segments
  statements <- segment_list |>
    purrr::map(list_index, data = data$dat) |>
    purrr::set_names(segment_list) |>
    data.table::rbindlist(idcol = 'segment') |>
    tibble::tibble() |>
    dplyr::mutate(across(matches("sample|vertical|index"), as.numeric))

  # function: filter each audience by vertical and index
  filter_index <- function(
    group,
    intensity,
    vertical,
    index1,
    index2,
    compare
  ) {
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
    tmp <- dplyr::full_join(
      all_statements,
      tmp,
      by = dplyr::join_by(segment, question, answer)
    ) |>
      dplyr::mutate(
        segment = stringr::str_to_title(stringr::str_replace_all(
          segment,
          "_",
          " "
        )),
        segment = paste0("<b>", segment, "</b>"),
        question = sapply(question, function(x) {
          paste(strwrap(x, 70), collapse = "<br>")
        })
      )
  }

  # filter all psychographics for tab 4
  filter_full_index <- function(
    group,
    intensity,
    vertical,
    index1,
    index2,
    compare
  ) {
    quo_groupindex <- dplyr::sym(glue::glue("{group}_index"))
    quo_groupvert <- dplyr::sym(glue::glue("{group}_vertical"))

    # keep all pscyhographic statments matching intensity
    all_psychographics <- data$dat |>
      dplyr::select(
        category_tier_1,
        category = category_tier_2,
        question,
        answer
      ) |>
      dplyr::filter(answer == intensity)

    if (compare == TRUE) {
      # keep statements matching vertical
      tmp <- data$dat |>
        dplyr::select(c(
          category_tier_1,
          category = category_tier_2,
          question,
          answer,
          dplyr::matches(group)
        )) |>
        dplyr::filter(
          answer == intensity &
            !!quo_groupvert >= vertical
          # (!!quo_groupindex <= index1 |
          #    !!quo_groupindex >= index2)
        )
    } else {
      # keep statements matching vertical and index
      tmp <- data$dat |>
        dplyr::select(c(
          category_tier_1,
          category = category_tier_2,
          question,
          answer,
          dplyr::matches(group)
        )) |>
        dplyr::filter(
          answer == intensity &
            !!quo_groupvert >= vertical &
            (!!quo_groupindex <= index1 |
              !!quo_groupindex >= index2)
        )
    }

    # # full join with all statements
    tmp <- dplyr::full_join(
      all_psychographics,
      tmp,
      by = dplyr::join_by(category_tier_1, category, question, answer)
    ) |>
      dplyr::mutate(
        category = paste0("<b>", category, "</b>"),
        question = sapply(question, function(x) {
          paste(strwrap(x, 70), collapse = "<br>")
        })
      )
  }

  agreement_count <- function(group, intensity, vertical, index1, index2) {
    segtest <- c('segment', "FALSE", "TRUE")
    quo_groupindex <- dplyr::sym(glue::glue("{group}_index"))
    quo_groupvert <- dplyr::sym(glue::glue("{group}_vertical"))

    neg_out <- statements |>
      dplyr::select(c(segment:answer, dplyr::matches(group))) |>
      dplyr::filter(answer == intensity) |>
      dplyr::count(
        segment,
        response = (!!quo_groupvert >= vertical & !!quo_groupindex <= index1)
      ) |>
      tidyr::pivot_wider(
        id_cols = segment,
        names_from = response,
        values_from = n
      )

    if (ncol(neg_out) < 3) {
      miss <- segtest[-match(colnames(neg_out), segtest)]
      neg_out <- neg_out |>
        dplyr::mutate({{ miss }} := 0) |>
        purrr::set_names(nm = c("segment", "insig", "sig")) |>
        dplyr::mutate(
          dplyr::across(insig:sig, ~ ifelse(is.na(.), 0, .)),
          count = sig + insig,
          neg_frac = sig / (sig + insig)
        ) |>
        dplyr::select(segment, count, neg_frac) |>
        purrr::set_names(
          nm = c("segment", "count", glue::glue("{group}_under-index"))
        )
    } else {
      neg_out <- neg_out |>
        purrr::set_names(nm = c("segment", "insig", "sig")) |>
        dplyr::mutate(
          dplyr::across(insig:sig, ~ ifelse(is.na(.), 0, .)),
          count = sig + insig,
          neg_frac = sig / (sig + insig)
        ) |>
        dplyr::select(segment, count, neg_frac) |>
        purrr::set_names(
          nm = c("segment", "count", glue::glue("{group}_under-index"))
        )
    }

    pos_out <- statements |>
      dplyr::select(c(segment:answer, dplyr::matches(group))) |>
      dplyr::filter(answer == intensity) |>
      dplyr::count(
        segment,
        response = (!!quo_groupvert >= vertical & !!quo_groupindex >= index2)
      ) |>
      tidyr::pivot_wider(
        id_cols = segment,
        names_from = response,
        values_from = n
      )

    if (ncol(pos_out) < 3) {
      miss <- segtest[-match(colnames(pos_out), segtest)]
      pos_out <- pos_out |>
        dplyr::mutate({{ miss }} := 0) |>
        purrr::set_names(nm = c("segment", "insig", "sig")) |>
        dplyr::mutate(
          dplyr::across(insig:sig, ~ ifelse(is.na(.), 0, .)),
          pos_frac = sig / (sig + insig)
        ) |>
        dplyr::select(segment, pos_frac) |>
        purrr::set_names(nm = c("segment", glue::glue("{group}_over-index")))
    } else {
      pos_out <- pos_out |>
        purrr::set_names(nm = c("segment", "insig", "sig")) |>
        dplyr::mutate(
          dplyr::across(insig:sig, ~ ifelse(is.na(.), 0, .)),
          pos_frac = sig / (sig + insig)
        ) |>
        dplyr::select(segment, pos_frac) |>
        purrr::set_names(nm = c("segment", glue::glue("{group}_over-index")))
    }

    tmp <- dplyr::left_join(neg_out, pos_out, by = dplyr::join_by(segment)) |>
      dplyr::mutate(
        segment = stringr::str_to_title(stringr::str_replace_all(
          segment,
          "_",
          " "
        )),
        segment = paste0("<b>", segment, "</b>")
      )
    return(tmp)
  }

  # create tibble to pmap across
  tmp_tbl <- tidyr::crossing(
    data$grp_def,
    intensity = intensity,
    vertical = vertical / 100,
    index1 = index1,
    index2 = index2,
    compare = compare
  )

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
    purrr::reduce(
      # dplyr::full_join,
      ~ dplyr::full_join(.x, .y, by = dplyr::join_by(segment, question, answer))
    )

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
    purrr::reduce(
      # dplyr::full_join,
      ~ dplyr::full_join(
        .x,
        .y,
        by = dplyr::join_by(category_tier_1, category, question, answer)
      )
    )

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
      dplyr::mutate(
        exclude = all(dplyr::c_across(dplyr::matches("index")) |> is.ou())
      ) |>
      dplyr::mutate(dplyr::across(
        dplyr::matches("sample|vert|index"),
        ~ ifelse(exclude == TRUE, NA, .)
      )) |>
      dplyr::select(-exclude)

    full_dat <- full_dat |>
      dplyr::rowwise() |>
      dplyr::mutate(
        exclude = all(dplyr::c_across(dplyr::matches("index")) |> is.ou())
      ) |>
      dplyr::mutate(dplyr::across(
        dplyr::matches("sample|weighted|vert|horiz|index"),
        ~ ifelse(exclude == TRUE, NA, .)
      )) |>
      dplyr::select(-exclude)
  } else {
    dat
    full_dat
  }

  if (empty == TRUE) {
    dat <- dat |>
      dplyr::filter(dplyr::if_any(
        dplyr::matches("sample|vert|index"),
        ~ !is.na(.)
      ))
    full_dat <- full_dat |>
      dplyr::filter(dplyr::if_any(
        dplyr::matches("sample|vert|index"),
        ~ !is.na(.)
      ))
  } else {
    dat
    full_dat
  }
  list(
    dat = dat,
    full_dat = full_dat,
    agree_matrix = agree_matrix,
    demos = data$demos,
    definitions = data$grp_def
  )
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
        lapply(
          audiences,
          th,
          colspan = 1,
          style = "text-align: center;
               border-bottom-width: 1px;
               border-bottom-style: solid; border-bottom-color: white"
        )
      ),
      tr(
        lapply(rep(c('Over-Index %'), length(audiences)), th)
      )
    )
  ))
  # 2. build the table
  DT::datatable(
    df,
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
      lengthChange = TRUE
    ),
    caption = glue::glue("Segment Summary: {unique(data$full_dat$answer)}")
  ) |>
    DT::formatStyle(columns = c('segment'), `text-align` = 'left') |>
    DT::formatPercentage(columns = stringr::str_detect(names(df), "over"), 1) |>
    DT::formatStyle(
      stringr::str_detect(names(df), "over"),
      fontWeight = DT::styleInterval(c(.399), c('', 'bold'))
    ) |>
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
        lapply(
          audiences,
          th,
          colspan = 3,
          style = "text-align: center;
               border-bottom-width: 1px;
               border-bottom-style: solid; border-bottom-color: white"
        )
      ),
      tr(
        lapply(rep(c('Sample', 'Vertical', "Index"), length(audiences)), th)
      )
    )
  ))
  # 2. build the table
  DT::datatable(
    df,
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
      lengthChange = TRUE
    ),
    caption = glue::glue(
      "Psychographics statements: {unique(data$full_dat$answer)}"
    )
  ) |>
    DT::formatStyle(
      columns = c('category', 'question'),
      `text-align` = 'left'
    ) |>
    DT::formatPercentage(columns = stringr::str_detect(names(df), "vert"), 1) |>
    DT::formatCurrency(
      stringr::str_detect(names(df), 'sample'),
      currency = "",
      mark = ",",
      digits = 0
    ) |>
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
        lapply(
          audiences,
          th,
          colspan = 3,
          style = "text-align: center;
               border-bottom-width: 1px;
               border-bottom-style: solid; border-bottom-color: white"
        )
      ),
      tr(
        lapply(rep(c('Sample', 'Vertical', "Index"), length(audiences)), th)
      )
    )
  ))
  # 2. build the table
  DT::datatable(
    data$dat,
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
      lengthChange = TRUE
    ),
    caption = glue::glue("Psychographics statements: {unique(data$dat$answer)}")
  ) |>
    DT::formatStyle(
      columns = c('segment', 'question'),
      `text-align` = 'left'
    ) |>
    DT::formatPercentage(stringr::str_detect(names(data$dat), "vert"), 1) |>
    DT::formatCurrency(
      stringr::str_detect(names(data$dat), 'sample'),
      currency = "",
      mark = ",",
      digits = 0
    ) |>
    DT::formatStyle(
      stringr::str_detect(names(data$dat), "index"),
      color = DT::styleInterval(c(91, 109), c('red', '', '#1D741B')),
      fontWeight = DT::styleInterval(c(91, 109), c('bold', '', 'bold')),
    ) |>
    DT::formatStyle(columns = colnames(data$dat), fontSize = '85%')
}
