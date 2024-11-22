# # fam_vec = c("m","f","s1","mgm","mgf","pgm","pgf")
# # n_fam = NULL
# # add_ind = TRUE
# # genetic_corrmat = diag(4)
# # full_corrmat = diag(4)
# # h2_vec = rep(.5,4)
# # phen_names = NULL
# # n_sim = 10
# # pop_prev = rep(.1,4)
# 
# sims = LTFHPlus::simulate_under_LTM_multi(n_sim = 10, genetic_corrmat = diag(10), full_corrmat = diag(10), h2_vec = rep(0.5,10),pop_prev = rep(.1, 10))
# 
# 
# # estimate_liability_multi(.tbl = sims$thresholds, h2_vec = rep(0.5, 10), genetic_corrmat = diag(10), full_corrmat = diag(10),
# #                          pid = "indiv_ID", fam_id = "fam_ID", phen_names = paste0("t", 1:10))
# 
# ntraits = 10
# .tbl = sims$thresholds
# h2_vec = rep(0.5, ntraits)
# #genetic_corrmat = diag(ntraits)
# genetic_corrmat = matrix(0.5, ncol = ntraits, nrow = ntraits)
# diag(genetic_corrmat) = 1
# #full_corrmat = diag(ntraits)
# full_corrmat = matrix(0.6, ncol = ntraits, nrow = ntraits)
# diag(full_corrmat) = 1
# 
# pid = "indiv_ID"
# fam_id = "fam_ID"
# role = "role"
# out = 1:2
# tol = 0.01
# phen_names = paste0("phenotype", 1:ntraits)
# 
# estimate_liability_multi_tmp <- function(.tbl = NULL,
#                                      family_graphs = NULL,
#                                      # family = NULL,
#                                      # threshs = NULL,
#                                      h2_vec,
#                                      genetic_corrmat,
#                                      full_corrmat,
#                                      phen_names = NULL, # phen_names does not do anything atm
#                                      pid = "PID",
#                                      fam_id = "fam_ID",
#                                      role = "role",
#                                      family_graphs_col = "fam_graph",
#                                      out = c(1),
#                                      tol = 0.01){
# 
#   # validating input-agnostic variables --------------------------------------
#   
#   # Turning pid, fam_id into strings
#   pid <- as.character(pid)
#   fam_id <- as.character(fam_id)
# 
#   # Checking that the heritability is valid
#   if (validate_proportion(h2_vec)) invisible()
# 
#   # Checking that all correlations are valid
#   if (validate_correlation_matrix(genetic_corrmat)) invisible()
#   if (validate_correlation_matrix(full_corrmat)) invisible()
# 
#   # Checking that tol is valid
#   if (!is.numeric(tol) && !is.integer(tol)) stop("The tolerance must be numeric!")
#   if (tol <= 0) stop("The tolerance must be strictly positive!")
# 
#   # Checking that out is either a character vector or a numeric vector
#   if (is.numeric(out)) {
# 
#     out <- intersect(out, c(1,2))
# 
#   } else if (is.character(out)) {
# 
#     out <- c("genetic", "full")[rowSums(sapply(out, grepl, x = c("genetic", "full"))) > 0]
#     out[out == "genetic"] <- 1
#     out[out == "full"] <- 2
#     out <- as.numeric(out)
#     
#   } else {
#     stop("out must be a numeric or character vector!")
#   }
# 
#   # Checking whether out is empty
#   if (length(out) == 0) {
# 
#     cat("Warning message: \n out is not of the required format! \n The function will return the estimated genetic liability!")
#     out <- c(1)
#     
#   }
# 
#   # Sorting out
#   out <- sort(out)
# 
#   # Now we can extract the number of phenotypes
#   n_pheno <- length(h2_vec)
# 
# 
#   # Validating input specific variables -------------------------------------
#   if ( !is.null(.tbl) ) {  #### .tbl input ####
#     
#     # Turning .tbl into a tibble
#     # if it is not of class tbl
#     if (!tibble::is_tibble(.tbl)) .tbl <- tibble::as_tibble(.tbl)
#     
#     # Turning role into string
#     role <- as.character(role)
#     
#     # Checking that .tbl has three columns named pid_col, fam_id and role
#     if (!(pid %in% colnames(.tbl))) stop(paste0("The column ", pid," does not exist in the tibble .tbl..."))
#     if (!(fam_id %in% colnames(.tbl))) stop(paste0("The column ", fam_id," does not exist in the tibble .tbl..."))
#     if (!(role %in% colnames(.tbl))) stop(paste0("The column ", role," does not exist in the tibble .tbl..."))
# 
#     # In addition, we check that the two columns lower and upper are present
#     if (any(!c("lower","upper") %in% gsub("_.*", "", colnames(.tbl), ignore.case = TRUE))) stop("The tibble .tbl must include columns named 'lower' and 'upper'!")
# 
#     # If the tibble consists of more than the required columns,
#     # we select only the relevant ones.
#     .tbl <- select(.tbl, !!as.symbol(fam_id), !!as.symbol(pid), !!as.symbol(role), tidyselect::starts_with("lower"), tidyselect::starts_with("upper"))
# 
#     # checking if the correct number of columns is present
#     if (ncol(.tbl) != (2*n_pheno + 3)) stop("Something is wrong with the number of phenotypes... \n
# The number of pairs of lower and upper thresholds is not equal to the number of phenotypes specified in h2_vec...\n
# Does all columns have the required names?")
# 
#     # We check whether all lower thresholds are
#     # smaller than or equal to the upper thresholds
#     for (pheno in phen_names) {
# 
# 
#       if (any(pull(.tbl, !!as.symbol(paste0("lower_", pheno))) > pull(.tbl, !!as.symbol(paste0("upper_", pheno))))) {
#         cat("Warning message: \n Some lower thresholds are larger than the corresponding upper thresholds! \n
# The lower and upper thresholds will be swapped...")
# 
#         swapping_indx <- which(pull(.tbl, !!as.symbol(paste0("lower_", pheno))) > pull(.tbl, !!as.symbol(paste0("upper_", pheno))))
# 
#         .tbl <- mutate(.tbl, !!as.symbol(paste0("lower_", pheno)) := ifelse(row_number() %in% swapping_indx, !!as.symbol(paste0("lower_", pheno)) + !!as.symbol(paste0("upper_", pheno)), !!as.symbol(paste0("lower_", pheno)))) %>%
#           mutate(., !!as.symbol(paste0("upper_", pheno)) := ifelse(row_number() %in% swapping_indx, !!as.symbol(paste0("lower_", pheno)) - !!as.symbol(paste0("upper_", pheno)), !!as.symbol(paste0("upper_", pheno)))) %>%
#           mutate(., !!as.symbol(paste0("lower_", pheno)) := ifelse(row_number() %in% swapping_indx, !!as.symbol(paste0("lower_", pheno)) - !!as.symbol(paste0("upper_", pheno)), !!as.symbol(paste0("lower_", pheno))))
#       }
#     }
# 
#     #  Extracting the (unique) family identifiers
#     fam_list <- unique(pull(.tbl, !!as.symbol(fam_id)))
# 
#   } else if ( !is.null(family_graphs) ) { #### Graph input ####
# 
#     #check if family_graphs is present, and if the pid column is present.
#     if ( !(pid %in% colnames(family_graphs)) ) {
#       stop(paste0("The column ", pid," does not exist in the tibble family_graphs."))
#     }
#     # checking if the family graph column present.
#     if ( !(family_graphs_col %in% colnames(family_graphs)) ) {
#       stop(paste0("The column ", family_graphs_col," does not exist in the tibble family_graphs."))
#     }
#     # extract graph attributes
#     graph_attrs = get.vertex.attribute((family_graphs %>% pull(!!as.symbol(family_graphs_col)))[[1]])
# 
#     # check if the upper and lower thresholds are present for each provided phenotype name in phen_names.
#     if ( !(any(paste(rep(c("lower", "upper"), length(phen_names)), rep(phen_names, each = 2), sep = "_") %in% names(graph_attrs))) ) {
#       stop("not all lower and upper columns are present as attributes in family_graph for a multi trait analysis.")
#     }
# 
#     # Extracting the (unique) family identifiers
#     fam_list <- unique(pull(family_graphs, !!as.symbol(pid)))
# 
#   } else ( stop("no valid input used.") )
# 
#   # how many workers are we using?
#   cat(paste0("The number of workers is ", future::nbrOfWorkers(), "\n"))
# 
#   # actual liability estimates happen below
#   gibbs_res <- future.apply::future_lapply(X = 1:length(fam_list), FUN = function(i){
#     
#     # current family id (proband id for graphs)
#     cur_fam_id = fam_list[i]
# 
#     ##### Ultimately, we get cov and temp_tbl from both of the cases below ##### 
#     
#     if ( !is.null(.tbl) ) {
#   
#       # Extracting the thresholds for all family members,
#       # including the thresholds for o and/or g,
#       # and all phenotypes
#       temp_tbl = filter(.tbl, !!as.symbol(fam_id) == cur_fam_id)
# 
#       # Extract the personal IDs and roles for all family members
#       pids  <- pull(temp_tbl, !!as.symbol(pid))
#       roles <- pull(temp_tbl, !!as.symbol(role))
# 
#       # Constructing the covariance matrix.
#       cov <- construct_covmat(fam_vec = roles, n_fam = NULL, add_ind = TRUE,
#                               genetic_corrmat = genetic_corrmat, full_corrmat = full_corrmat,
#                               h2 = h2_vec, phen_names = phen_names)
# 
# 
#       # Sometimes the constructed matrix was not positive definite, leading to computational
#       # issues in the gibbs sampler. This check ensures the matrix will be PD.
# 
#       cov_PD = correct_positive_definite_simplified(covmat = cov)
#       cov = cov_PD$covmat
# 
#       # If the thresholds for o and/or g are missing, they will
#       # be set to -Inf (lower threshold) and Inf (upper
#       # threshold)
# 
#       temp_tbl = add_missing_roles_for_proband(
#         temp_tbl = temp_tbl,
#         role = role,
#         cur_roles = roles,
#         cur_fam_id = cur_fam_id,
#         pid = pid,
#         fam_id = fam_id,
#         phen_names = phen_names
#       )
# 
#       # Ordering temp_tbl to match the order in cov.
#       first_indx <- match(c("g","o"), pull(temp_tbl, !!as.symbol(role)))
#       other_indx <- setdiff(1:length(pull(temp_tbl, !!as.symbol(role))), first_indx)
# 
#       temp_tbl <- mutate(temp_tbl, !!as.symbol(role) := factor(!!as.symbol(role), levels = pull(temp_tbl,!!as.symbol(role))[c(first_indx, other_indx)])) %>%
#         arrange(., !!as.symbol(role))
# 
# 
#       # Now, we need to lengthen the data
#       
#       # extract lower thresholds
#       fam_threshs <- select(temp_tbl, -c(starts_with("upper"))) %>%
#         tidyr::pivot_longer(., cols = starts_with("lower"), names_to = "phenotype", values_to = "lower") %>%
#         mutate(., phenotype = gsub("lower_","",phenotype))
# 
#       # extract upper thresholds
#       fam_threshs <- select(temp_tbl, -c(starts_with("lower"))) %>%
#         tidyr::pivot_longer(., cols = starts_with("upper"), names_to = "phenotype", values_to = "upper") %>%
#         mutate(., phenotype = gsub("upper_","",phenotype)) %>%
#         # join upper and left thresholds
#         left_join(fam_threshs,., by = stats::setNames(c(fam_id,pid,role,"phenotype"), c(fam_id,pid,role,"phenotype"))) %>%
#         mutate(., phenotype = factor(phenotype, levels = phen_names)) %>%
#         # order the tibble, such that it matches the covariance matrix
#         arrange(., phenotype, !!as.symbol(role))
# 
#     } else if ( !is.null(family_graphs) ) {
#       
#       # extracting current (local) family graph
#       cur_fam_graph = family_graphs[[family_graphs_col]][[i]]
#       
#       # construct covariance and extract threshold information from graph.
#       cov_obj = graph_based_covariance_construction_multi(fam_id = fam_id,
#                                                           pid = pid,
#                                                           cur_proband_id = cur_fam_id,
#                                                           cur_family_graph = cur_fam_graph,
#                                                           h2_vec = h2_vec,
#                                                           genetic_corrmat = genetic_corrmat,
#                                                           phen_names = phen_names)
#       # cov and temp_tbl are ordered during construction, but lengthening messes 
#       # with the ordering of temp_tbl.
#       
#       # threshold information
#       temp_tbl = cov_obj$temp_tbl
#       newOrder = cov_obj$newOrder
#       
#       
#       cov_PD = correct_positive_definite_simplified(covmat = cov_obj$cov)
#       cov = cov_PD$covmat
#       #correction_itr = cov_PD$nitr
#       
#       # Now, we need to lengthen the data
# 
#       # extract lower thresholds
#       fam_threshs <- select(temp_tbl, -c(starts_with("upper"))) %>%
#         tidyr::pivot_longer(., cols = starts_with("lower"), names_to = "phenotype", values_to = "lower") %>%
#         mutate(., phenotype = gsub("lower_","",phenotype))
# 
#       # extract upper thresholds
#       fam_threshs <- select(temp_tbl, -c(starts_with("lower"))) %>%
#         tidyr::pivot_longer(., cols = starts_with("upper"), names_to = "phenotype", values_to = "upper") %>%
#         mutate(., phenotype = gsub("upper_","",phenotype)) %>%
#         # join upper and left thresholds
#         left_join(fam_threshs,., by = stats::setNames(c(fam_id,pid,"phenotype"), c(fam_id,pid,"phenotype"))) %>%
#         # mutate(., phenotype = factor(phenotype, levels = phen_names)) %>%
#         # order the tibble, such that it matches the covariance matrix
#         slice(match(newOrder, paste0(!!as.symbol(pid), "_", phenotype)))
# 
#     } else { stop("How did you even get here?") }
#     
#     
#     # Setting the variables needed for Gibbs sampler
#     lower = pull(fam_threshs,lower)
#     upper = pull(fam_threshs,upper)
#     fixed <- (upper - lower) < 1e-04
#     std_err <- matrix(Inf, ncol =  length(out), nrow = n_pheno)
#     colnames(std_err) <- c("genetic", "full")[out]
#     n_gibbs <- 1
# 
#     # And change the variable out, as we need to extract the
#     # genetic and/or full liability for each phenotype.
#     # And as all thresholds in fam_threshs are ordered according
#     # to the phenotype and the role, we need to extract every
#     # (number of individuals)'th entry starting from the entries
#     # specified in out.
#     updated_out <- sort(sapply(out, function(k) k + nrow(temp_tbl)*(0:(n_pheno - 1))))
# 
#     # Running Gibbs sampler
#     while (any(std_err > tol)) {
# 
#       if (n_gibbs == 1) {
# 
#         est_liabs <- rtmvnorm.gibbs(n_sim = 1e+05, covmat = cov, lower = lower, upper = upper,
#                                     fixed = fixed, out = updated_out, burn_in = 1000) %>%
#           `colnames<-`(paste0(rep(c("genetic", "full")[out], n_pheno),"_", rep(phen_names, each = length(out)))) %>%
#           tibble::as_tibble()
# 
#       } else {
# 
#         est_liabs <- rtmvnorm.gibbs(n_sim = 1e+05, covmat = cov, lower = pull(fam_threshs, lower), upper = pull(fam_threshs, upper),
#                                     fixed = fixed, out = updated_out, burn_in = 1000) %>%
#           `colnames<-`(paste0(rep(c("genetic", "full")[out], n_pheno),"_", rep(phen_names, each = length(out)))) %>%
#           tibble::as_tibble() %>%
#           bind_rows(est_liabs,.)
#       }
# 
#       # Computing the standard error
#       std_err[1:n_pheno,1:length(out)] <- matrix(batchmeans::bmmat(est_liabs)[,2], ncol =  length(out), nrow = n_pheno, byrow = TRUE)
#       # Adding one to the counter
#       n_gibbs <- n_gibbs + 1
#     }
# 
#     # If all standard errors are below the tolerance,
#     # the estimated liabilities as well as the corresponding
#     # standard error can be returned
#     res <- tibble::as_tibble(batchmeans::bmmat(est_liabs), rownames = "out") %>%
#       tidyr::pivot_longer(., cols = c(est,se)) %>%
#       mutate(., name = paste0(out, "_", name), .keep = "unused") %>%
#       tibble::deframe(.)
# 
#     # formatting and returning result
#     tibble(!!as.symbol(fam_id) := cur_fam_id,
#            !!as.symbol(pid) := cur_fam_id,
#            !!!stats::setNames(res, names(res)))
# 
#   }, future.seed = TRUE) %>%
#     do.call("bind_rows",.)
# 
#   return(gibbs_res)
# }
# 
# 
# 
# 
# 
# # -------------------------------------------------------------------------
# 
# 
# library(dplyr)
# library(LTFHPlus)
# library(igraph)
# library(stringr)
# 
# # covariance matrix construction is slow, but it works.
# # it is slow due to many (nested) for loops
# 
# ntraits = 10
# gen_mat = matrix(0.5, nrow = ntraits, ncol = ntraits)
# diag(gen_mat) = 1
# 
# 
# tbl_est = estimate_liability_multi_tmp(.tbl = sims$thresholds,
#                                        h2_vec = rep(0.5, 10),
#                                        genetic_corrmat = gen_mat,
#                                        full_corrmat = gen_mat,
#                                        pid = "indiv_ID",
#                                        fam_id = "fam_ID",
#                                        phen_names = paste0("phenotype", 1:ntraits),
#                                        out = 1)
# 
# tbl_est2 = estimate_liability_multi_tmp(.tbl = sims$thresholds,
#                                         h2_vec = rep(0.5, 10),
#                                         genetic_corrmat = gen_mat,
#                                         full_corrmat = gen_mat,
#                                         pid = "indiv_ID",
#                                         fam_id = "fam_ID",
#                                         phen_names = paste0("phenotype", 1:ntraits),
#                                         out = 1:2)
# 
# 
# # -------------------------------------------------------------------------
# 
# 
# 
# 
# family_tbl = tribble(
#   ~id, ~momcol, ~dadcol,
#   "pid", "mom", "dad",
#   "sib", "mom", "dad",
#   "mhs", "mom", "dad2",
#   "phs", "mom2", "dad",
#   "mom", "mgm", "mgf",
#   "dad", "pgm", "pgf",
#   "dad2", "pgm2", "pgf2",
#   "paunt", "pgm", "pgf",
#   "pacousin", "paunt", "pauntH",
#   "hspaunt", "pgm", "newpgf",
#   "hspacousin", "hspaunt", "hspauntH",
#   "puncle", "pgm", "pgf",
#   "pucousin", "puncleW", "puncle",
#   "maunt", "mgm", "mgf",
#   "macousin", "maunt", "mauntH",
#   "hsmuncle", "newmgm", "mgf",
#   "hsmucousin", "hsmuncleW", "hsmuncle"
# )
# ntraits = 10
# 
# thrs =  tibble(
#   id = family_tbl %>% select(1:3) %>% unlist() %>% unique()) %>%
#   bind_cols(
#     lapply(1:ntraits, function(j) {
#   tibble(!!as.symbol(paste0("lower_phenotype", j)) := sample(c(-Inf, 2), size = nrow(.), replace = TRUE),
#          !!as.symbol(paste0("upper_phenotype", j)) := sample(c(2, Inf), size = nrow(.), replace = TRUE))
# }) %>% bind_cols
# )
# 
# 
# graph = prepare_graph(.tbl = family_tbl,
#                       fcol = "dadcol",
#                       mcol = "momcol",
#                       thresholds = thrs,
#                       icol = "id")
# get.vertex.attribute(graph)
# 
# 
# 
# proband_graphs = tibble(
#   fam_ID = V(graph)$name,
#   fam_graph = make_ego_graph(graph = graph,
#                              order = 2,
#                              nodes = fam_ID)
# )
# 
# family_graphs = proband_graphs
# family_graphs_col = "fam_graph"
# 
# 
# graph_est = estimate_liability_multi_tmp(family_graphs = family_graphs,
#                                          h2_vec = rep(0.5, 10),
#                                          genetic_corrmat = gen_mat,
#                                          full_corrmat = gen_mat,
#                                          pid = "indiv_ID",
#                                          fam_id = "fam_ID",
#                                          phen_names = paste0("phenotype", 1:ntraits),
#                                          out = 1)
# 
# graph_est2 = estimate_liability_multi_tmp(family_graphs = family_graphs,
#                                           h2_vec = rep(0.5, 10),
#                                           genetic_corrmat = gen_mat,
#                                           full_corrmat = gen_mat,
#                                           pid = "indiv_ID",
#                                           fam_id = "fam_ID",
#                                           phen_names = paste0("phenotype", 1:ntraits),
#                                           out = 1:2)
# 
# # helper functions --------------------------------------------------------
# 
# 
# graph_based_covariance_construction_multi = function(fam_id,
#                                                      pid,
#                                                      cur_proband_id,
#                                                      cur_family_graph,
#                                                      h2_vec,
#                                                      genetic_corrmat,
#                                                      phen_names,
#                                                      add_ind = TRUE) {
#   # only calculate number of traits once
#   ntrait = length(phen_names)
# 
#   # constructing tibble with ids and thresholds
#   temp_tbl = as_tibble(get.vertex.attribute(cur_family_graph)) %>%
#     rename(!!as.symbol(pid) := name) %>%
#     mutate(!!as.symbol(fam_id) := cur_proband_id) %>%
#     relocate(!!as.symbol(fam_id), !!as.symbol(pid))
# 
#   # add genetic liability if required
#   if (add_ind) {
#     temp_tbl = tibble(
#       !!as.symbol(fam_id) := pull(temp_tbl, !!as.symbol(fam_id))[1],
#       !!as.symbol(pid)    := paste0(cur_proband_id, "_g")
#     ) %>%
#       bind_cols(
#         tibble::tibble(!!!c(stats::setNames(rep(-Inf, ntrait), paste0("lower_", phen_names)), # all lower
#                             stats::setNames(rep( Inf, ntrait), paste0("upper_", phen_names))))# all upper
#         ) %>%
#       bind_rows(# add to original temp_tbl
#         .,
#         temp_tbl
#       )
#   }
# 
#   fam_size = nrow(temp_tbl)
#   cov = matrix(NA, nrow = fam_size * ntrait, ncol = fam_size * ntrait)
# 
#   # graph based kinship matrix, with genetic liability added
#   for (i in 1:ntrait) {
#     i_ind = 1:fam_size + fam_size * (i - 1)
#     for (j in i:ntrait) {
#       j_ind = 1:fam_size + fam_size * (j - 1)
#       if (i == j) {
#         cov[i_ind, j_ind] <- get_kinship(fam_graph = cur_family_graph,
#                                          h2 = h2_vec[i],
#                                          add_ind = add_ind,
#                                          index_id = cur_proband_id)
#       } else {
#         cov[i_ind, j_ind] <- cov[j_ind, i_ind] <- get_kinship(fam_graph = cur_family_graph,
#                                                               h2 = sqrt(h2_vec[i] * h2_vec[j]) * genetic_corrmat[i,j],
#                                                               add_ind = add_ind,
#                                                               index_id = cur_proband_id, fix_diag = F)
# 
#       }
#     }
#   }
#   
#   for_name = get_kinship(fam_graph = cur_family_graph,
#                          h2 = h2_vec[1],
#                          add_ind = add_ind,
#                          index_id = cur_proband_id)
#   colnames(cov) <- rownames(cov) <- paste(rep(colnames(for_name), ntrait), rep(phen_names, each = fam_size), sep = "_")
# 
#   # Now that we have extracted all the relevant information, we
#   # only need to order the observations before we can run
#   # Gibbs sampler, as g and o need to be the first two
#   # observations.
# 
#   # get current order within one phenotype (same order for all)
#   rnames = str_subset(rownames(cov), paste0(phen_names[1], "$")) %>%
#     #removing phenotype name for just the IDs
#     str_replace_all(paste0("_", phen_names[1]), "")
# 
# 
#   to_put_first = paste0(cur_proband_id, c("_g", ""))
#   to_put_last  = setdiff(rnames, to_put_first)
#   withinPhenotypeOrder = c(to_put_first, to_put_last)
#   newOrder = paste0(rep(withinPhenotypeOrder, ntrait), "_", rep(phen_names, each = fam_size))
# 
#   # new ordering
#   temp_tbl = temp_tbl[match(withinPhenotypeOrder, pull(temp_tbl, !!as.symbol(pid))),]
#   cov  = cov[newOrder,newOrder]
# 
#   return(list(temp_tbl = temp_tbl, cov = cov, newOrder = newOrder))
# }
# 
# 
# 
# # -------------------------------------------------------------------------
# 
# #genetic_corrmat = diag(ntraits)
# genetic_corrmat = matrix(0.5, ncol = ntraits, nrow = ntraits)
# diag(genetic_corrmat) = 1
# full_corrmat = diag(ntraits)
# # full_corrmat = matrix(0.6, ncol = ntraits, nrow = ntraits)
# # diag(full_corrmat) = 1
# 
# cov = construct_covmat(fam_vec = roles, n_fam = NULL, add_ind = TRUE,
#                        genetic_corrmat = genetic_corrmat, full_corrmat = full_corrmat,
#                        h2 = h2_vec, phen_names = phen_names)
# eigen_vals = eigen(cov)
# eigen_vals$values
# 
# cov = correct_positive_definite(covmat = cov, correction_limit = 300)
# 
# # The covariance mats can be made PD. However, I believe there is a problem with the 
# # way full_corrmat is used. I need to verify that the formulaes are correct.
# 
# 
# # -------------------------------------------------------------------------
# 
# 
# correct_positive_definite_simplified = function(covmat, correction_limit = 100, correction_val = 0.99) {
#   eigen_val = eigen(covmat)
#   
#   if ( any(eigen_val$values < 0) ) {
#     og_diag = diag(covmat)
#     n = 0
#     
#     while ( any(eigen(covmat)$values < 0) & n <= correction_limit ) {
#       covmat = covmat*correction_val
#       diag(covmat) = og_diag
#       n = n + 1
#     }
#     
#     if (eigen(eigen_val$values < 0)) stop("Unable to enforce a positive definite covariance matrix.")
#     return(list(covmat = covmat, nitr = n))
#   } else {
#     return(list(covmat = covmat, nitr = 0))
#   }
# }
# 
# 
# cov = construct_covmat(fam_vec = roles, n_fam = NULL, add_ind = TRUE,
#                        genetic_corrmat = genetic_corrmat, full_corrmat = diag(ntraits),
#                        h2 = h2_vec, phen_names = phen_names)
# eigen_vals = eigen(cov)
# eigen_vals$values
# 
# cov_corrected = correct_positive_definite_simplified(covmat = cov)
# 
# library(ggplot2)
# m_melted <- reshape::melt(cov)
# ggplot(m_melted, aes(x = X1, y = X2, fill = as.numeric(value))) +
#   geom_tile()
# 
# m_melted_PD = reshape::melt(cov_corrected$covmat)
# ggplot(m_melted_PD, aes(x = X1, y = X2, fill = as.numeric(value))) +
#   geom_tile()
# 
# 
# m_melted2 = reshape::melt((cov - cov_corrected$covmat) / cov)
# ggplot(m_melted2, aes(x = X1, y = X2, fill = as.numeric(value))) +
#   geom_tile()
# 
# cov_corrected$covmat/cov
# 0.99^cov_corrected$nitr
