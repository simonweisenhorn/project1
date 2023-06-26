rmarkdown::render(input = 'index.Rmd',
                  output_format = "github_document",
                  output_file = 'README.md',
                  output_options = list(html_preview=FALSE,
                                        toc = TRUE,
                                        toc_depth = 2))
