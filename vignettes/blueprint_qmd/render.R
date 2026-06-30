
list.files(dir_path, pattern = "\\.Rmd$$", full.names = TRUE) |>
  vapply(function(x) file.remove(x), logical(1L))

dir_path <- file.path("vignettes", "blueprint_qmd")

quarto::quarto_render(
  file.path(dir_path, "*.qmd"),
  as_job = FALSE,
  output_format = "md"
)

list.files(dir_path, pattern = "\\.md$", full.names = TRUE) |>
  vapply(
    function(x) {
      new_x <- gsub("\\.md$", ".Rmd", x)
      old_qmd <- gsub("\\.md$", ".qmd", x)

      # rename file
      file.rename(from = x, to = new_x)

      print(x)
      # append header
      header_boundaries <- which(grepl("^-+$", readLines(old_qmd)))
      if (NROW(header_boundaries) >= 2) {
        header <- readLines(old_qmd)[seq(header_boundaries[1], header_boundaries[2])]
        valid_header <- tryCatch(
          {
            yaml::yaml.load(header)
            TRUE
          },
          error = function(err) FALSE
        )
        new_contents <- c(header, "", readLines(new_x))
        writeLines(new_contents, new_x)
        TRUE
      }
      TRUE
    },
    logical(1L)
  )
