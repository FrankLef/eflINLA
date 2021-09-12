from_dir <- "C:/Users/Ephel/Documents/R/win-library/4.1/rethinking/data"
to_dir <- "C:/Users/Public/MyR/Packages/eflINLA/tests/testthat/testdata/rethinking"

# copy csv files ----------------------------------------------------------
the_files <- list.files(path = from_dir, pattern = "[.]csv$")
# the_files
the_data <- lapply(X = the_files, FUN = function(x) {
  read.csv2(file = file.path(from_dir, x), dec = ".")
})
names(the_data) <- sub(pattern = "[.]csv$", replacement = "", x = the_files)
# str(the_data[["Howell1"]])

invisible(lapply(X = names(the_data), FUN = function(nm) {
  assign(nm, the_data[[nm]])
  save(list = nm, file = file.path(to_dir, paste0(nm, ".rda")))
}))



# copy rda files ----------------------------------------------------------

the_files <- list.files(path = from_dir, pattern = "[.]rda$")
# the_files

file.copy(from = file.path(from_dir, the_files), to = to_dir, overwrite = TRUE)
