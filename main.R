library(data.table)
library(dplyr)
library(rvest)
all <- read_xml("ccs3000ruM_ok_all.xml")
xml_structure(all)
# doc = xmlTreeParse("ccs3000ruM_ok_all.xml", useInternal = TRUE)
# 
# CBL
cbl_vars <- xml_nodes(all, "panel cbl item") %>% xml_attr(attr = "var")
cbl_text <- xml_nodes(all, "panel cbl item text") %>% xml_text()
cbl_fst_text <- xml_nodes(all, "panel label text :nth-child(1)") %>% xml_text()
repair_encoding(cbl_text[i])

enc <- function(txt) {
    res <- vector("character")
    suppressMessages(
    for (i in 1:length(txt)) {
        res[i] <- ifelse(grepl("Error", try(repair_encoding(txt[i]), silent = T)),
                         txt[i],
                         repair_encoding(txt[i]))
    }
    )
    res
}

# RBL
rbl_f <- function(rbl = rbl) {
    l <- list()
    l[[1]] <- xml_nodes(rbl, "rbl") %>% xml_attr(attr = "var")
    l[[2]] <- xml_nodes(rbl, "rbl item") %>% xml_attr(attr = "value") %>% as.integer()
    l[[3]] <- enc(xml_nodes(rbl, "rbl item text") %>% xml_text())
    paste("val lab", l[[1]], paste(l[[2]], paste0("'", l[[3]], "'"), collapse = " "), ".")
}
# CBL
cbl_f <- function(cbl = cbl) {
    l <- list()
    l[[1]] <- xml_nodes(cbl, "cbl item") %>% xml_attr(attr = "var")
    l[[2]] <- enc(xml_nodes(cbl, "cbl item text") %>% xml_text())
    paste("var lab", l[[1]], paste0("'", l[[2]]), "'", ".")
    paste("val lab", paste(l[[1]], collapse = " "), "0 '-' 1 'Да'", ".")
}
# TB
tb_f <- function(tb) {
    l <- list()
    l[[1]] <- xml_nodes(tb, "tb") %>% xml_attr(attr = "var")
    l[[2]] <- is.na(xml_nodes(tb, "tb") %>% xml_attr(attr = "mask"))
    l[[3]] <- xml_nodes(tb, "tb item text") %>% xml_text()
    l
}

######### by groups
groups <- xml_nodes(all, "group question")

for (i in 1:length(groups)) {
    if (xml_nodes(groups[i], "rbl") %>% length() > 0) {
        cat(rbl_f(groups[i]), sep = "\n")
    }
    if (xml_nodes(groups[i], "cbl") %>% length() > 0) {
        cat(cbl_f(groups[i]), sep = "\n")
    }
    if (xml_nodes(groups[i], "tb") %>% length() > 0) {
        
    }
    
}


# groups[var == 1]
# for (i in 1:length(groups)) var[i] <- length(xml_children(groups[i]))
# xml_children(xml_siblings(groups[[1]])[1])


