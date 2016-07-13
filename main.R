library(data.table)
library(dplyr)
library(rvest)
all <- read_xml("ccs3000ruM_ok_all.xml")
# xml_structure(all)
# doc = xmlTreeParse("ccs3000ruM_ok_all.xml", useInternal = TRUE)
# 
# CBL
# cbl_vars <- xml_nodes(all, "panel cbl item") %>% xml_attr(attr = "var")
# cbl_text <- xml_nodes(all, "panel cbl item text") %>% xml_text()
# cbl_fst_text <- xml_nodes(all, "panel label text :nth-child(1)") %>% xml_text()
# repair_encoding(cbl_text[i])

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
rbl_f <- function(rbl) {
    l <- list()
    l[[1]] <- xml_nodes(rbl, "rbl") %>% xml_attr(attr = "var")
    l[[2]] <- xml_nodes(rbl, "rbl item") %>% xml_attr(attr = "value") %>% as.integer()
    l[[3]] <- enc(xml_nodes(rbl, "rbl item text") %>% xml_text())
    paste("val lab", l[[1]], paste(l[[2]], paste0("'", l[[3]], "'"), collapse = " "), ".")
}
# CBL
cbl_f <- function(cbl) {
    l <- list()
    l[[1]] <- xml_nodes(cbl, "cbl item") %>% xml_attr(attr = "var")
    l[[2]] <- enc(xml_nodes(cbl, "cbl item text") %>% xml_text())
    return(c(paste("var lab", l[[1]], paste0("'", l[[2]]), "'", "."),
    paste("val lab", paste(l[[1]], collapse = " "), "0 '-' 1 'Да'", ".")))
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
groups <- xml_nodes(all, "group")
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

groups <- xml_nodes(all, "group question")
for (i in 1:length(groups)) {
    crnt_panels <- xml_nodes(groups[i], "panel") # %>% xml_text() %>% repair_encoding()
    type_res <- c(xml_nodes(groups[i], "rbl") %>% length(), xml_nodes(groups[i], "cbl") %>% length(), xml_nodes(groups[i], "tb") %>% length())
    df_typ <- data.frame(rbl = type_res[1], cbl = type_res[2], tb = type_res[3])
    if (crnt_panels %>% length() == 3) {
        if (df_typ$cbl == 2) {
            fst_cbl_lab <- xml_nodes(xml_nodes(groups[i], "panel")[2], "label") %>% xml_text() %>%  enc
            fst_cbl_lab <- paste0(strsplit(fst_cbl_lab, "?", fixed = T )[[1]], collapse = " - ")
            fst_cbl_val <- xml_nodes(xml_nodes(groups[i], "panel")[2], "item") %>% xml_text() %>%  enc %>% .[1]
            fst_cbl_nam <- xml_nodes(groups[i], "panel item")[1] %>% xml_attr("var")
            cat(paste("var lab", fst_cbl_nam, "'", paste(fst_cbl_lab, fst_cbl_val, sep = '_'),"'."), sep = "\n")
            
            snd_cbl_lab <- xml_nodes(xml_nodes(groups[i], "panel")[3], "label") %>% xml_text() %>%  enc
            snd_cbl_lab <- paste(strsplit(fst_cbl_lab, " - ", fixed = T)[[1]][1], snd_cbl_lab, sep = " - ")
            snd_cbl_nam <- xml_nodes(xml_nodes(groups[i], "panel")[3], "item")[1] %>% xml_attr("var")
            snd_cbl_val <- xml_nodes(xml_nodes(groups[i], "panel")[3], "item") %>% xml_text() %>%  enc %>% .[1]
            cat(paste("var lab", snd_cbl_nam, "'", paste(snd_cbl_lab, snd_cbl_val, sep = '_'),"'."), sep = "\n")
            
        } else if (df_typ$rbl == 2) {
            fst_rbl_lab <- xml_nodes(xml_nodes(groups[i], "panel")[2], "label") %>% xml_text() %>%  enc
            temp <- sub("\n([^\n]*)$","", fst_rbl_lab)
            parts <- strsplit(fst_rbl_lab, "\n")
            fst_rbl_lab <- paste0(temp, "-", sub("    ", "", parts[[1]][length(parts[[1]])]))
            snd_rbl_lab <- paste0(temp, "-", xml_node(xml_nodes(groups[i], "panel")[3], "label") %>% xml_text() %>% enc())
            
            fst_rbl_nam <- xml_nodes(groups[i], "panel rbl")[1] %>% xml_attr("var") 
            snd_rbl_nam <- xml_nodes(groups[i], "panel rbl")[2] %>% xml_attr("var") 
            cat(paste("var lab", fst_rbl_nam, "'", fst_rbl_lab, "'."), sep = "\n")
            cat(paste("var lab", snd_rbl_nam, "'", snd_rbl_lab, "'."), sep = "\n")
        } else if (df_typ$rbl == 1) {
            
        } else if (df_typ$cbl == 1) {
            
        }
        
    } else if (crnt_panels %>% length() == 2) {
        if (df_typ$rbl > 0) cat(suppressWarnings(paste("var lab", 
                                                       xml_nodes(groups[i], "panel rbl")[1] %>% xml_attr(attr = "var"), "'",
                                                       xml_nodes(xml_nodes(groups[i], "panel")[2], "label text") %>% xml_text() %>% enc() , "'.")), sep = "\n")
        if (df_typ$cbl == 1) cat(suppressWarnings(paste("var lab", 
                                                        xml_node(groups[i], "cbl item") %>% xml_attr(attr = "var"), "'",
                                                        paste0(xml_nodes(groups[i], "label text") %>% xml_text() %>% enc() %>% .[3] ,
                                                               "_",
                                                               xml_node(groups[i], "cbl item text") %>% xml_text() %>%  enc()),
                                                        "'.")), sep = "\n")
    }
}

# groups[var == 1]
# for (i in 1:length(groups)) var[i] <- length(xml_children(groups[i]))
# xml_children(xml_siblings(groups[[1]])[1])
# types <- c("rbl", "cbl", "tb")
# types[as.logical(type_res)]
# length(types[type_res])
# type_res

