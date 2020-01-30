path.assistant <- function(path) {
    if (!dir.exists(path)) {
        print(paste0("'", path, "'", " does not exist. Therefore, new directory is created."))
        dir.create(path, recursive = TRUE)
    } else {
        print(paste0("'", path, "'", " exists, Therefore, use an existing path."))
    }
}
