# sync to googledrive

libs <- c("googledrive", "tidyverse")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

# updating the manuscript files
googledrive::drive_deauth()
rmds_pdfs <- drive_find(pattern = "Jones_", n_max = 10) 

for(i in 1:nrow(rmds_pdfs)){
  drive_find(pattern = rmds_pdfs$name[i], n_max = 1) %>%
    drive_update(rmds_pdfs$name[i])
  
}

# updating the R scripts
rs <- c("a_prep_mjcb.R", "b_data_exploration_mjcb.R", "c_js_figs.R", "d_sem_mods.R",
        "ggplot_sem.R", "gdrive_sync.R")

for(i in 1:length(rs)){
  drive_find(pattern = rs[i], n_max = 1) %>%
    drive_update(paste0("R/",rs[i]))
  
}
