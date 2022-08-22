#Data Mine PPTX


library("officer")

Test <- read_pptx("23SEPT2021 Lab Meeting TP Slides.pptx")

content <- pptx_summary(Test)

#slide_summary(Test)

par_data <- subset(content, content_type %in% "paragraph") 
par_data <- par_data[, c("doc_index", "style_name", 
                         "text", "level", "num_id") ]
par_data$text <- with(par_data, {
  substr(
    text, start = 1, 
    stop = ifelse(nchar(text)<30, nchar(text), 30) )
})
par_data

