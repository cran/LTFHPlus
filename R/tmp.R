# library(dplyr)
# library(LTFHPlus)
# library(igraph)
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
# thrs =  tibble(
#   id = family_tbl %>% select(1:3) %>% unlist() %>% unique(),
#   lower = sample(c(-Inf, 2), size = length(id), replace = TRUE),
#   upper = sample(c(2, Inf), size = length(id), replace = TRUE))
# 
# 
# 
# # .tbl = family_tbl
# # fcol = "dadcol"
# # mcol = "momcol"
# # icol = "id"
# # lower_col = "lower"
# # upper_col = "upper"
# # missingID_patterns = "^0$"
# 
# graph = prepare_graph(.tbl = family_tbl,
#                       fcol = "dadcol",
#                       mcol = "momcol",
#                       thresholds = thrs,
#                       icol = "id")
# 
# left_join(thrs, as_tibble(get.vertex.attribute(graph)), by = c("id" = "name")) %>%
#   summarise(mean(lower.x == lower.y), mean(upper.x == upper.y))
# 
# 
# proband_graphs = tibble(
#   famID = V(graph)$name,
#   fam_graph = make_ego_graph(graph = graph,
#                              order = 2,
#                              nodes = famID)
# )
# 
# thres_info = tibble(
#   pid = V(graph)$name,
#   lower = sample(c(-Inf, 2), size = length(pid), replace = TRUE),
#   upper = sample(c(2, Inf), size = length(pid), replace = TRUE)
# )
# 
# .tbl = proband_graphs %>%
#   mutate(pid = lapply(fam_graph, function(x) V(x)$name)) %>%
#   select(-fam_graph) %>%
#   tidyr::unnest(pid) %>%
#   left_join(thres_info)
# 
# # input
# h2 = 0.5
# pid = "pid" # personal ID (within families)
# fam_id = "famID" # identifies family, typically proband ID
# family = NULL
# threshs = NULL
# .tbl = NULL
# # could role be used as differentiating variable such that we do not need more vars to construct graph families?
# role = NULL
# out = c(1)
# tol = 0.01
# get_kinship(proband_graphs$fam_graph[[1]], h2 = 1, index_id = "pid", add_ind = T)
# family_graphs = proband_graphs
# family_graphs_col = "fam_graph"
# 
# estimate_liability_single <- function(.tbl = NULL,
#                                       family_graphs = NULL,
#                                       h2 = 0.5,
#                                       pid = "PID",
#                                       fam_id = "fam_ID",
#                                       family_graphs_col = "fam_graph",
#                                       role = NULL,
#                                       out = c(1),
#                                       tol = 0.01){
# 
# 
#   # validating input-agnostic variables --------------------------------------
# 
#   # Turning pid and fam_id into strings
#   pid <- as.character(pid)
#   fam_id <- as.character(fam_id)
# 
#   # Checking that the heritability is valid
#   if (validate_proportion(h2)) invisible()
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
#   } else {
#     stop("out must be a numeric or character vector!")
#   }
# 
#   # Checking whether out is empty
#   if (length(out) == 0) {
#     cat("Warning message: \n out is not of the required format! \n The function will return the estimated genetic liability!")
#     out <- c(1)
#   }
# 
#   # Sorting out
#   out <- sort(out)
# 
# 
#   # Validating input specific variables -------------------------------------
# 
#   if ( !is.null(.tbl) ) { #### .tbl input ####
# 
#     # Turning .tbl into a tibble
#     # if it is not of class tbl
#     if (!is.null(.tbl) && !tibble::is_tibble(.tbl))  .tbl <- tibble::as_tibble(.tbl)
# 
#     # role (as string) must be supplied
#     if (is.null(role)) stop("role must be specified.")
#     # if role is supplied, convert to string
#     if (!is.null(role)) role <- as.character(role)
# 
#     # Checking that .tbl has three columns named pid, fam_id and role
#     if (!(pid %in% colnames(.tbl))) stop(paste0("The column ", pid," does not exist in the tibble .tbl..."))
#     if (!(fam_id %in% colnames(.tbl))) stop(paste0("The column ", fam_id," does not exist in the tibble .tbl..."))
#     if (!(role %in% colnames(.tbl))) stop(paste0("The column ", role," does not exist in the tibble .tbl..."))
# 
#     # In addition, we check that two columns named lower and upper are present
#     if (any(!c("lower","upper") %in% colnames(.tbl))) stop("The tibble .tbl must include two columns named 'lower' and 'upper'!")
# 
#     # If the tibble consists of more than the required columns,
#     # we select only the relevant ones.
#     .tbl <- select(.tbl,
#                    !!as.symbol(fam_id),
#                    !!as.symbol(pid),
#                    !!as.symbol(role),
#                    tidyselect::starts_with("lower"),
#                    tidyselect::starts_with("upper"))
# 
# 
#     # Finally, we also check whether all lower thresholds are
#     # smaller than or equal to the upper thresholds
#     if (any(pull(.tbl, lower) > pull(.tbl, upper))) {
#       cat("Warning message: \n Some lower thresholds are larger than the corresponding upper thresholds! \n
#   The lower and upper thresholds will be swapped...")
# 
#       swapping_indx <- which(pull(.tbl, lower) > pull(.tbl, upper))
# 
#       .tbl$lower[swapping_indx] <- .tbl$lower[swapping_indx] + .tbl$upper[swapping_indx]
#       .tbl$upper[swapping_indx] <- .tbl$lower[swapping_indx] - .tbl$upper[swapping_indx]
#       .tbl$lower[swapping_indx] <- .tbl$lower[swapping_indx] - .tbl$upper[swapping_indx]
#     }
# 
#     # Extracting the (unique) family identifiers
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
# 
#     # extract attributes from graph
#     graph_attrs = get.vertex.attribute((family_graphs %>% pull(!!as.symbol(family_graphs_col)))[[1]])
# 
#     if ( !(any(c("lower", "upper") %in% names(graph_attrs))) ) {
#       stop("lower and upper are not present as attributes in family_graph.")
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
#     if ( !is.null(family_graphs) ) { # family_graph based covariance construction
# 
#       # extract current (local) family graph
#       cur_fam_graph = family_graphs[[family_graphs_col]][[i]]
# 
#       # construct covariance and extract threshold information from graph.
#       cov_obj = graph_based_covariance_construction(fam_id = fam_id,
#                                                     pid = pid,
#                                                     cur_proband_id = cur_fam_id,
#                                                     cur_family_graph = cur_fam_graph,
#                                                     h2 = h2)
# 
#       # cov and temp_tbl are ordered during construction
# 
#       # threshold information
#       temp_tbl = cov_obj$temp_tbl
# 
#       # check whether covariance matrix is positive definite
#       # correct if needed.
#       cov_PD = correct_positive_definite_simplified(covmat = cov_obj)
#       cov = cov_PD$covmat
# 
# 
#     } else { # role based covariance construction
# 
#       # extract all with current family ID.
#       temp_tbl = filter(.tbl, !!as.symbol(fam_id) == cur_fam_id)
# 
#       # Extract the personal numbers and roles for all family members
#       pids  <- pull(temp_tbl, !!as.symbol(pid))
#       roles <- pull(temp_tbl, !!as.symbol(role))
# 
#       # Constructing the covariance matrix.
#       cov_obj <- construct_covmat(fam_vec = roles, n_fam = NULL, add_ind = TRUE, h2 = h2)
# 
#       # check for whether covariance matrix is positive definite
#       # correct if needed.
#       cov_PD = correct_positive_definite_simplified(covmat = cov_obj)
#       cov = cov_PD$covmat
# 
#       # adding missing roles (of either g or o)
#       temp_tbl = add_missing_roles_for_proband(temp_tbl = temp_tbl,
#                                                role = role,
#                                                cur_roles = roles,
#                                                cur_fam_id = cur_fam_id,
#                                                pid = pid,
#                                                fam_id = fam_id)
# 
#       # Now that we have extracted all the relevant information, we
#       # only need to order the observations before we can run
#       # Gibbs sampler, as g and o need to be the first two observations.
# 
#       first_indx <- match(c("g","o"), pull(temp_tbl, !!as.symbol(role)))
#       other_indx <- setdiff(1:length(pull(temp_tbl, !!as.symbol(role))), first_indx)
#       temp_tbl <- temp_tbl[c(first_indx, other_indx),]
#     }
# 
#     # Setting the variables needed for Gibbs sampler
#     fixed <- (pull(temp_tbl,upper) - pull(temp_tbl,lower)) < 1e-04
#     std_err <- rep(Inf, length(out))
#     names(std_err) <- c("genetic", "full")[out]
#     n_gibbs <- 1
# 
#     # Running Gibbs sampler
#     while(any(std_err > tol)){
# 
#       if(n_gibbs == 1){
# 
#         est_liabs <- rtmvnorm.gibbs(n_sim = 1e+05, covmat = cov, lower = pull(temp_tbl, lower), upper = pull(temp_tbl, upper),
#                                     fixed = fixed, out = out, burn_in = 1000) %>%
#           `colnames<-`(c("genetic", "full")[out]) %>%
#           tibble::as_tibble()
# 
#       }else{
# 
#         est_liabs <- rtmvnorm.gibbs(n_sim = 1e+05, covmat = cov, lower = pull(temp_tbl, lower), upper = pull(temp_tbl, upper),
#                                     fixed = fixed, out = out, burn_in = 1000) %>%
#           `colnames<-`(c("genetic", "full")[out]) %>%
#           tibble::as_tibble() %>%
#           bind_rows(est_liabs)
#       }
# 
#       # Computing the standard error
#       std_err <- batchmeans::bmmat(est_liabs)[,2]
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
#            genetic_est = res[[1]],
#            genetic_se  = res[[2]])
# 
#   }, future.seed = TRUE) %>%
#     do.call("bind_rows", .)
# 
#   return(gibbs_res)
# }
# 
# profvis::profvis({
# 
# # it seems a problem arises when names are the same or contains subsets of oneanother, e.g. paunt, hspaunt
# estimate_liability_single(
#   h2 = .5,
#   pid = "pid",
#   fam_id = "famID",
#   family_graphs = proband_graphs,
#   family_graphs_col = "fam_graph")
# 
# })
# 
# sims = simulate_under_LTM(n_sim = 10)
# estimate_liability_single(
#   .tbl = sims$thresholds,
#   h2 = 0.5,
#   pid = "indiv_ID",
#   fam_id = "fam_ID",
#   role = "role"
# )
# # it seems a problem arises when names are the same or contains subsets of oneanother, e.g. paunt, hspaunt
# estimate_liability_single(
#   .tbl = .tbl %>%
#     filter(famID %in% c("pid")) %>%
#     mutate(role = c("o", "s1","mhs", "phs", "m", "f", "pau1", "pau2", "mau1", "mgm", "pgm", "mgf", "pgf")),
#   h2 = .5,
#   pid = "pid",
#   fam_id = "famID",
#   role = "role",
#   #family_graphs = proband_graphs,
#   #family_graphs_col = "fam_graph"
#   )
# 
# 
# #   -----------------------------------------------------------------------
# 
# 
# graph_based_covariance_construction = function(fam_id,
#                                                pid,
#                                                cur_proband_id,
#                                                cur_family_graph,
#                                                h2,
#                                                add_ind = TRUE) {
#   # constructing tibble with ids and thresholds
#   temp_tbl = as_tibble(get.vertex.attribute(cur_family_graph)) %>%
#     rename(!!as.symbol(pid) := name) %>%
#     mutate(!!as.symbol(fam_id) := !!as.symbol(pid)) %>%
#     relocate(!!as.symbol(fam_id), !!as.symbol(pid))
# 
#   # add genetic liability if required
#   if (add_ind) {
#     temp_tbl = temp_tbl %>%
#       bind_rows(
#         tibble(
#           !!as.symbol(fam_id) := cur_proband_id,
#           !!as.symbol(pid) := paste0(cur_proband_id,"_g"),
#           lower = -Inf,
#           upper = Inf),
#         .
#       )
#   }
# 
# 
#   # graph based kinship matrix, with genetic liability added
#   cov = get_kinship(fam_graph = cur_family_graph,
#                     h2 = h2,
#                     add_ind = add_ind,
#                     index_id = cur_proband_id)
# 
#   # Now that we have extracted all the relevant information, we
#   # only need to order the observations before we can run
#   # Gibbs sampler, as g and o need to be the first two
#   # observations.
#   rnames = rownames(cov)
#   to_put_first = paste0(cur_proband_id, c("_g", ""))
#   to_put_last  = setdiff(rnames, to_put_first)
#   newOrder = c(to_put_first, to_put_last)
# 
#   # new ordering
#   temp_tbl = temp_tbl[match(newOrder, pull(temp_tbl, !!as.symbol(pid))),]
#   cov  = cov[newOrder,newOrder]
#   return(list(temp_tbl = temp_tbl, covmat = cov))
# }
# 
# 
# add_missing_roles_for_proband = function(temp_tbl, role, cur_roles, cur_fam_id, pid, fam_id, phen_names = NULL) {
#   # role types to check for, centered on proband
#   to_check_for = c("g", "o")
# 
#   # roles is already calculated; are they present?
#   to_be_added = setdiff(to_check_for,   cur_roles)
#   present     = intersect(to_check_for, cur_roles)
# 
#   # if some present, extract individual ID, if not, get family ID
#   if (length(present) > 0 ) {
#     i_pid = (temp_tbl %>% filter(!!as.symbol(role) == present) %>% pull(!!as.symbol(pid)))[1]
#   } else {
#     i_pid = pull(temp_tbl, !!as.symbol(fam_id))[1]
#   }
#   # suffixes of roles to be added
#   id_suffixes = paste0("_",to_be_added) %>% stringr::str_replace_all(., "_o", "")
# 
#   if ( is.null(phen_names) ) { # single trait
#     # construct tibble with desired roles
#     tibble(
#       !!as.symbol(fam_id) := pull(temp_tbl, !!as.symbol(fam_id))[1],
#       !!as.symbol(pid)    := paste0(i_pid, id_suffixes),
#       !!as.symbol(role)   := to_be_added,
#       lower = rep(-Inf, length(to_be_added)),
#       upper = rep( Inf, length(to_be_added))
#     ) %>%
#       bind_rows(., temp_tbl)
# 
#   } else { # multi trait
#     # constructs id rows, then adds lower and upper thresholds from phen_names provided
#     tibble(
#       !!as.symbol(fam_id) := pull(temp_tbl, !!as.symbol(fam_id))[1],
#       !!as.symbol(pid)    := paste0(i_pid, id_suffixes),
#       !!as.symbol(role)   := to_be_added
#     ) %>%
#       bind_cols(
#         tibble::tibble(!!!c(stats::setNames(rep(-Inf, n_pheno), paste0("lower_", phen_names)),
#                             stats::setNames(rep(Inf, n_pheno), paste0("upper_", phen_names))))) %>%
#       bind_rows(
#         .,
#         temp_tbl
#       )
#   }
# }
# 
# 
# # -------------------------------------------------------------------------
# 
# add_missing_roles_for_proband(temp_tbl = temp_tbl,
#                               role = "role",
#                               cur_roles = temp_tbl %>% pull(role),
#                               cur_fam_id ,
#                               pid = pid,
#                               phen_names = phen_names,
#                               fam_id = fam_id)
# 
# add_missing_roles_for_proband(temp_tbl = temp_tbl %>% filter( !(role %in% c("g", "o")) ),
#                               role = "role",
#                               cur_roles = temp_tbl %>% filter( !(role %in% c("g", "o")) ) %>% pull(role),
#                               cur_fam_id ,
#                               pid = pid,
#                               phen_names = phen_names,
#                               fam_id = fam_id)
# 
# 
# 
# 
# # -------------------------------------------------------------------------
# 
# 
# add_missing_roles_for_proband(temp_tbl = temp_tbl,
#                               role = "role",
#                               cur_roles = temp_tbl$role,
#                               cur_fam_id ,
#                               pid = pid,
#                               fam_id = fam_id)
# 
# temp_tbl2 = add_missing_roles_for_proband(temp_tbl = temp_tbl %>% filter( !(role %in% c("g", "o")) ),
#                               role = "role",
#                               cur_roles = temp_tbl %>% filter( !(role %in% c("g", "o")) ) %>% pull(role),
#                               cur_fam_id ,
#                               pid = pid,
#                               fam_id = fam_id)
# 
# add_missing_roles_for_proband(temp_tbl = temp_tbl2,
#                               role = "role",
#                               cur_roles = temp_tbl2 %>% pull(role),
#                               cur_fam_id ,
#                               pid = pid,
#                               fam_id = fam_id)
# 
# temp_tbl %>% filter( !(role %in% c("g", "o")) )
# 
# if("g" %in% pull(temp_tbl, !!as.symbol(role))) i_pid <- pull(filter(temp_tbl, !!as.symbol(role)== "g"), !!as.symbol(pid))
# if(!"g" %in% pull(temp_tbl, !!as.symbol(role))) i_pid <- paste0(pull(temp_tbl, !!as.symbol(fam_id))[1], "_o")
# 
# bind_rows(tibble::tibble(!!as.symbol(fam_id) := pull(temp_tbl, !!as.symbol(fam_id))[1],
#                          !!as.symbol(pid) := i_pid,
#                          !!as.symbol(role) := "o",
#                          lower = -Inf,
#                          upper = Inf),
#           temp_tbl)
# 
# 
# genetic_corrmat <- matrix(0.4, 3, 3)
# diag(genetic_corrmat) <- 1
# full_corrmat <- matrix(0.6, 3, 3)
# diag(full_corrmat) <- 1
# #
# sims <- simulate_under_LTM(fam_vec = c("m","f"), n_fam = NULL, add_ind = TRUE,
# genetic_corrmat = genetic_corrmat, full_corrmat = full_corrmat, h2 = rep(.5,3),
# n_sim = 10, pop_prev = rep(.1,3))
# #
# # full_covmat = lapply(sims[-which(names(sims) == "thresholds")], function(x) {
# #    dplyr::select(x[[1]], dplyr::starts_with("o") & dplyr::ends_with("status"))
# #    })
# # full_covmat = cov(dplyr::bind_cols(full_covmat))
# # diag(full_covmat) = 1
# 
# estimate_liability_multi(.tbl = sims$thresholds,
#                          h2_vec = rep(.5,3),
#                          genetic_corrmat = genetic_corrmat,
#                          full_corrmat = full_corrmat,
#                          pid = "indiv_ID",
#                          fam_id = "fam_ID",
#                          role = "role",
#                          out = c(1),
#                          tol = 0.01,
#                          phen_names = paste0("phenotype", 1:3))
# genetic_corrmat <- matrix(0.4, 3, 3)
# diag(genetic_corrmat) <- 1
# full_corrmat <- matrix(0.6, 3, 3)
# diag(full_corrmat) <- 1
# #
# sims <- simulate_under_LTM(fam_vec = c("m","f"), n_fam = NULL, add_ind = TRUE,
#                                genetic_corrmat = genetic_corrmat, full_corrmat = full_corrmat, h2 = rep(.5,3),
#                                n_sim = 10, pop_prev = rep(.1,3))
# estimate_liability_multi(.tbl = sims$thresholds, h2_vec = rep(.5,3),
#                              genetic_corrmat = genetic_corrmat, full_corrmat = full_corrmat,
#                              pid = "indiv_ID", fam_id = "fam_ID", role = "role", out = c(1),
#                              tol = 0.01, phen_names = paste0("phenotype", 1:3))
