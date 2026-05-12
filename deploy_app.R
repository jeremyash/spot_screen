deploy_dev <- function() {
  all_files <- list.files(
    ".",
    recursive = TRUE,
    all.files = FALSE,
    no.. = TRUE
  )
  
  exclude <- grepl(
    paste(
      c(
        "^r8_forests/",
        "^\\.git/",
        "^\\.github/",
        "^archive/",
        "^cache/",
        "^ndfd_region8/",
        "^sfog_pngs/",
        "\\.DS_Store$"
      ),
      collapse = "|"
    ),
    all_files
  )
  
  rsconnect::deployApp(
    appDir = ".",
    appName = "spot-screen-dev",
    appFiles = all_files[!exclude]
  )
}

deploy_dev()