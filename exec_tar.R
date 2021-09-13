###################
# Execute targets #
###################

library(targets)
library(tarchetypes)
library(tictoc)

###############

# execute & explore results
tic()
# tar_destroy()
tar_make(names = c("epaESP", "epa", "parametres"))
tar_make(names = c("F1_taula_probs_grups_ESP",
                   "F1_ERTO_sect_ESP",
                   "F1_TGatur0", "F1_TGatur1", "F1_TGcob",
                   "F1_dur_atur", "F1_dur_erto"))
pr <- tar_read(parametres)
tar_make(names = c("F2_bsl_dt", "F2_myrefs", "F2_dades"))
tar_make(names = c("F3_simrefs", "F3_mreslist", "F3_sublists"))
(ncores <- parallel::detectCores(logical = T))
tar_make_future(names = c("sims"),
                workers = (ncores-1))
future::plan("sequential")
tar_make(names = c("F3_resmat"))
tar_make(names = c("F3_escenari", "F4_dd_dashb"))
tar_make(names = c("F3_myseed", "F5_contrafactic"))
tar_make(names = c("dashboard1", "pla_explotacio"))
tar_make(names = c("tot_be"))
toc()

beepr::beep(8)

# review
tar_visnetwork()

# debugging
tar_make(names = c("F2_dades"),
         callr_function = NULL)

tar_read(tot_be)
View(tar_meta(fields = warnings))
