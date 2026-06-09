# HTML rendering quick fix
# Энэ script нь C:/quarto_sites/family_medicine_health_site доторх .qmd файлуудын
# HTML мөрүүдийн indentation-ийг засаж, <div> код хэлбэрээр харагдах асуудлыг арилгана.

project_dir <- "C:/quarto_sites/family_medicine_health_site"
setwd(project_dir)

qmd_files <- list.files(project_dir, pattern = "\\.qmd$", recursive = TRUE, full.names = TRUE)

for (f in qmd_files) {
  x <- readLines(f, encoding = "UTF-8", warn = FALSE)
  x <- ifelse(grepl("^\\s*<", x), sub("^\\s+", "", x), x)
  writeLines(enc2utf8(x), f, useBytes = TRUE)
}

if (nzchar(Sys.which("quarto"))) {
  system("quarto render")
  file.create(file.path("docs", ".nojekyll"))
}

message("DONE: HTML rendering fixed. Дараа нь git add -A; git commit; git push хийнэ.")
