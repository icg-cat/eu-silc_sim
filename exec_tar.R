###################
# Executa targets #
###################

library(targets)
library(tarchetypes)
library(tictoc)
# library(clustermq)
# library(future)

###############

# executa i explora resultats
tic()
tar_destroy()
tar_make(names = c("epaESP", "epa", "parametres"))
tar_make(names = c("F1_taula_probs_grups_ESP",
                   "F1_ERTO_sect_ESP",
                   "F1_TGatur0", "F1_TGatur1", "F1_TGcob",
                   "F1_dur_atur", "F1_dur_erto"))
pr <- tar_read(parametres)
tar_make(names = c("F2_bsl_dt", "F2_myrefs", "F2_dades"))
tar_make(names = c("F3_simrefs", "F3_mreslist", "F3_sublists"))
# tar_make_clustermq(names = c("sims"),
#                    workers = (ncores-1))
#stopCluster(cl)
(ncores <- parallel::detectCores(logical = T))
tar_make_future(names = c("sims"),
                workers = (ncores-1))
future::plan("sequential")
# future:::ClusterRegistry("stop")
tar_make(names = c("F3_resmat"))
tar_make(names = c("F3_escenari", "F4_dd_dashb"))
tar_make(names = c("F3_myseed", "F5_contrafactic"))
tar_make(names = c("dashboard1", "pla_explotacio", "tules_contrafactic"))
tar_make(names = c("tot_be"))
tar_make(names = c("titol", "tupper"))
toc()

beepr::beep(8)

# revisió preliminar
tar_visnetwork()

# debugging
tar_make(names = c("F2_dades"),
         callr_function = NULL)



tar_read(tot_be)
View(tar_meta(fields = warnings))


# cleanup
# targets::tar_destroy() # removes the _targets/ data store completely, deleting all the results from tar_make() except for external files. Use it if you intend to start the pipeline from scratch without any trace of a previous run.
tar_prune() # deletes the data and metadata of all the targets no longer present in your current _targets.R file


tar_delete(tules_contrafactic)
tar_make(names = c("tules_contrafactic"))
