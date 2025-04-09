#### Model ####
#' @title Ranking model
#' @param vector_entry the initial vector of items that is re-rank
#' @param times the vector of N iteration, a seq(1, N, 1)
#' @param ps the probability ps
#' @param pd the probability pd
#' @param alpha the real number for a non uniform choice of items. If alpha = 0, the choice is uniform
#' @returns a list of N. Each list represent a MAJ of jumping or diffusion re-ordered list
#' @author Julie Gravier, based on Marc Barthelemy's FORTRAN function

f_ranking_model <- function(vector_entry, times, ps, pd, alpha) {
  
  # probabilities of jumping or diffusion or stay still
  ps_size <- round(x = ps*length(times), digits = 0)
  pdiff_size <- round(x = (1-ps)*pd*length(times), digits = 0)
  # pstill_size <- (length(times) - (ps_size + pdiff_size))
  proba_alpha <- ( (1-alpha)/(length(vector_entry)^(1-alpha)) )*vector_entry^(-alpha)
  
  # moving items in list
  list_vectors <- list()
  list_vectors[[1]] <- vector_entry
  
  for (i in 1:length(times)) {
    
    if (i <= ps_size) {
      ## sampling jumping
      s_init <- sample(x = list_vectors[[1]], size = 1, replace = FALSE, prob = proba_alpha)
      s_jump_output <- sample(x = list_vectors[[1]], size = 1, replace = FALSE, prob = proba_alpha)
      
      
      ### Jumping and re-ranking:
      if (purrr::is_empty(s_init)) {
        ### do nothing
      } else {
        vector_init <- list_vectors[[i]]
        rank_init <- list_vectors[[i]]
        vector_output <- list_vectors[[i]]
        posinvec_init <- which(vector_init %in% s_init[1])
        posinvec_ouput <- which(vector_init %in% s_jump_output[1])
        
        if (s_init[1] == s_jump_output[1]) {
          ############### do nothing
        } else if (which(vector_init %in% s_jump_output[1]) - which(vector_init %in% s_init[1]) == 1) { # if position output - input = 1
          # is the case when next one to each other but output is > than input
          # do > permutation
          vector_init[posinvec_ouput] <- s_init[1]
          vector_init[posinvec_init] <- s_jump_output[1]
          
        } else if (which(vector_init %in% s_init[1]) - which(vector_init %in% s_jump_output[1]) == 1) { # inverse
          # do > permutation
          vector_init[posinvec_ouput] <- s_init[1]
          vector_init[posinvec_init] <- s_jump_output[1]
          
        } else {
          mininvec <- min(posinvec_init, posinvec_ouput)
          maxinvec <- max(posinvec_init, posinvec_ouput)
          
          # update rank for elements with rank between sampling init and output
          if (posinvec_ouput < posinvec_init) {
            maxinvecrevu <- maxinvec-1
            j <- vector_init[mininvec:maxinvecrevu]
            minvec2 <- mininvec+1
            maxinvecrevu2 <- maxinvecrevu+1
            vector_output[minvec2:maxinvecrevu2] <- j 
          } else {
            mininvecrevu <- mininvec+1
            j <- vector_init[mininvecrevu:maxinvec]
            mininvecrevu2 <- mininvecrevu-1
            maxinvec2 <- maxinvec-1
            vector_output[mininvecrevu2:maxinvec2] <- j 
          }
          
          rank_init[which(rank_init %in% s_init[1])] <- s_jump_output[1]
          vector_init[which(vector_init %in% s_jump_output[1])] <- s_init[1]
          
          # update rank list
          if (posinvec_ouput < posinvec_init) {
            mininvecrevu <- mininvec+1
            vector_init[mininvecrevu:maxinvec] <- vector_output[mininvecrevu:maxinvec]
          } else {
            maxinvecrevu <- maxinvec-1
            vector_init[mininvec:maxinvecrevu] <- vector_output[mininvec:maxinvecrevu]
            
          }
          
        }
        
        list_vectors[[i+1]] <- vector_init
        #print(paste0("sample jump init: ", s_init[1], "   ----   sample jump output: ", s_jump_output[1]))
        vector_final_jumping <- list_vectors
        
      }
      
    } else if (i > ps_size & i <= (pdiff_size + ps_size)) { # from times t > jump to end of diffusion
      
      # if ps = 0
      if (ps_size == 0) {
        list_vectors_diff <- list()
        list_vectors_diff[[1]] <- vector_entry
        
      } else {
        list_vectors_diff <- vector_final_jumping
      }
      
      ### Diffusion re-ranking:
      for (i in (ps_size+1):(pdiff_size + ps_size)) {
        
        ## sampling diffusion
        s_init_pd <- sample(x = list_vectors_diff[[1]], size = 1, replace = FALSE, prob = proba_alpha)
        
        vector_init <- list_vectors_diff[[i]]
        posinvec_init <- which(vector_init %in% s_init_pd[1])
        
        # two possibilities of outputs: + or - 1
        posinvec_ouput <- c(posinvec_init-1, posinvec_init+1)
        # case when initial position in list is min or max: in those case posinvec_output[i] = NA
        if (posinvec_ouput[1] < min(vector_init)) { # case when posinvec init is the first element of the list
          posinvec_ouput <- posinvec_ouput[2]
          replacement <- vector_init[posinvec_ouput]
        } else if (posinvec_ouput[2] > max(vector_init)) { # case when posinvec init is the last element of the list
          posinvec_ouput <- posinvec_ouput[1]
          replacement <- vector_init[posinvec_ouput]
        } else {
          # proba 1/2 to select
          posinvec_ouput <- sample(x = posinvec_ouput, size = 1, replace = FALSE)
          replacement <- vector_init[posinvec_ouput] 
        }
        
        # permutation
        vector_init[posinvec_ouput] <- s_init_pd[1]
        vector_init[posinvec_init] <- replacement
        
        list_vectors_diff[[i+1]] <- vector_init
        #print(paste0("sample diffusion init: ", s_init_pd[1], "   ----   sample diffusion output: ", replacement))
        vector_final_diffusion <- list_vectors_diff
      }
      
    }
  }
  
  # output: final vector
  if (exists("list_vectors_diff")) { # does the list exist?
    vector_final <- vector_final_diffusion
  } else {
    vector_final <- vector_final_jumping
  }
  
  return(vector_final)
  
}


#### Metrics ####
#' @title calculus of F turnover in theoretical model, renamed phi
#' @param vector_entry the initial vector of items that will be re-rank
#' @param list_entry the list of results of ranking model
#' @returns a tibble of two variables: N0/N and F
#' @author Julie Gravier and Marc Barthelemy

f_calculus_F <- function(vector_entry, list_entry, N){
  # compute all turnovers. Theoretically: sum F <=> T when the probability to jump elsewhere in the list = 1
  vecdiff <- vector()
  for (i in 2:(length(list_entry))) {
    if (identical(list_entry[[i]], list_entry[[i-1]]) == FALSE) {
      vecdiff[i] <- 1
    }
  }
  sum_F <- sum(vecdiff, na.rm = TRUE)
  
  # creation of F turnover as the probability p = N0/N to cross the barrier N0/N
  output_f <- tibble()
  for (i in 1:length(vector_entry)) {
    liste_vec <- vector()
    # Each times: identification if items cross the N0/N border
    for (t in seq(2, length(list_entry), 1)) {
      # comparing list t and t-1
      extract_differences <- list_entry[[t]][1:i] %in% list_entry[[t-1]][1:i] #
      # count differences (when items == FALSE)
      n_f <- length(extract_differences[extract_differences == FALSE])
      liste_vec[t] <- n_f
    }
    # creation of output tibble
    output_f <- output_f %>%
      bind_rows(tibble(
        N0_N = i/length(vector_entry),
        Fresult = sum(liste_vec, na.rm = TRUE)/sum_F
      ))
  }
  return(output_f)
}


#' @title calculus of N barrier N0/N crossing in data (Fsum), renamed phi sum
#' @param vector_entry the initial vector of items that will be re-rank
#' @param times N times of observation
#' @param list_entry the list of results of ranking model
#' @param N size of ranking list N
#' @returns a tibble
#' @author Julie Gravier and Marc Barthelemy

f_calculus_F_data <- function(vector_entry, times, list_entry, N, vec_not_continuous = FALSE){
  output_f <- tibble()
  for (i in 1:length(vector_entry)) {
    liste_vec_total <- vector()
    liste_vec <- vector()
    # Each times: identification if items cross the N0/N border
    for (t in seq(2, length(list_entry), 1)) {
      # comparing list t and t-1
      extract_differences <- list_entry[[t]][1:i] %in% list_entry[[t-1]][1:i] #
      # count differences (when items == FALSE)
      n_f <- length(extract_differences[extract_differences == FALSE])
      liste_vec[t] <- n_f
      # total difference
      if (length(list_entry[[t]]) == length(list_entry[[t-1]])) {
        totdiff <- list_entry[[t]] == list_entry[[t-1]]
      } else if (length(list_entry[[t]]) > length(list_entry[[t-1]])) {
        long <- length(list_entry[[t-1]])
        totdiff <- list_entry[[t]][1:long] == list_entry[[t-1]]
      } else {
        long <- length(list_entry[[t]])
        totdiff <- list_entry[[t]] == list_entry[[t-1]][1:long]
      }
      ndiff <- length(totdiff[totdiff == FALSE])
      liste_vec_total[t] <- ndiff
    }
    
    # creation of output tibble
    output_f <- output_f %>%
      bind_rows(tibble(
        N0_N = if(vec_not_continuous == TRUE){vector_entry[i]/N} else {i/N},
        Fsum = sum(liste_vec, na.rm = TRUE),
        Ftotdiff = sum(liste_vec_total, na.rm = TRUE),
        Fresult = Fsum/Ftotdiff,
        times = times
      ))
  }
  return(output_f)
}


#' @title Overlapping
#' @param study_list the initial list of tibbles of ranking
#' @param Nzero is overlapping considering N in T or one/multiple N0 (default is TRUE)
#' @param sequence_of_Nzero sequence of N0 size of lists (a number or a sequence)
#' @param frequence is TRUE (default) for overlapping frequency output
#' @returns a tibble of overlapping indices from T-Final with previous T
#' @author Julie Gravier

f_overlapping <- function(study_list, Nzero = TRUE, sequence_of_Nzero, frequence = TRUE) {
  
  # from T-Final to previous T
  seq_reverse <- rev(seq(1, length(study_list), 1))
  
  # match T-Final with previous T
  output_overlapping <- tibble()
  
  
  # Default, N0 = TRUE
  if (Nzero == TRUE) {
    
    # choice of a N0 sequence (one number or a sequence)
    if (is.null(sequence_of_Nzero)) {
      
      print("Sequence of N0 should be non null")
      
    }
    
    else {
      
      for (i in 1:length(study_list)-1) {
        
        # compute overlapping for any N0 sequence
        for (seqN0 in 1:length(sequence_of_Nzero)) {
          # size of N0
          size <- sequence_of_Nzero[seqN0]
          
          # compute overlapping
          matching_vector <- match(x = study_list[[seq_reverse[1]]]$rowid[1:size], table = study_list[[seq_reverse[i+1]]]$rowid[1:size])
          overlapping <- 1 - (sum(is.na(matching_vector))/length(matching_vector)) # sum of NA
          overlappingnotfreq <- length(matching_vector) - sum(is.na(matching_vector))
          
          # create output tibble
          if (frequence == TRUE) {
            output_overlapping <- output_overlapping %>%
              bind_rows(
                tibble(overlapping_index = overlapping,
                       time_t_minus = i,
                       time_t_reality = study_list[[seq_reverse[i+1]]]$date[1],
                       Nzero_size = sequence_of_Nzero[seqN0])
              )
          } else {
            output_overlapping <- output_overlapping %>%
              bind_rows(
                tibble(overlapping_index = overlappingnotfreq,
                       time_t_minus = i,
                       time_t_reality = study_list[[seq_reverse[i+1]]]$date[1],
                       Nzero_size = sequence_of_Nzero[seqN0])
              )
          }
          
        }
        
      }
      
    }
  }
  
  # when N0 = FALSE
  else {
    
    for (i in 1:length(study_list)-1) {
      # compute overlapping
      matching_vector <- match(x = study_list[[seq_reverse[1]]]$rowid, table = study_list[[seq_reverse[i+1]]]$rowid)
      overlapping <- 1 - (sum(is.na(matching_vector))/length(matching_vector)) # sum of NA
      overlappingnotfreq <- length(matching_vector) - sum(is.na(matching_vector))
      
      # create output tibble
      if (frequence == TRUE) {
        output_overlapping <- output_overlapping %>%
          bind_rows(
            tibble(overlapping_index = overlapping,
                   time_t_minus = i,
                   time_t_reality = study_list[[seq_reverse[i+1]]]$date[1])
          )
      } else {
        output_overlapping <- output_overlapping %>%
          bind_rows(
            tibble(overlapping_index = overlappingnotfreq,
                   time_t_minus = i,
                   time_t_reality = study_list[[seq_reverse[i+1]]]$date[1])
          )
      }
    }
  }
  
  # output
  if (Nzero == TRUE) {
    return(output_overlapping %>% arrange(Nzero_size, desc(time_t_reality )))
  } else {
    return(output_overlapping)
  }
  
}


#### Fit formulas ####

#' @title fit formula of phi with power law distribution
#' @param N0_N probability p = N0/N
#' @param N size of ranking list N
#' @details
#' Formula of F(p) is used to fit model with data via nlstools.
#' \eqn{F(p) = p_s((1-p)p^{1-\alpha} + p(1-p^{1-\alpha})) + (1-p_s)p_d\frac{p^{-\alpha}}{N}}
#' @author Marc Barthelemy and Julie Gravier

formula_phi_asymetric <- as.formula(Fresult ~ ps * ( (N0_N^(1-alpha)) * (1-N0_N) + ( 1-N0_N^(1-alpha) ) * N0_N ) +
                                    (1 - ps) * pd * N0_N^(-alpha) * (1/N) )

#' @title fit formula of phi with exponential distribution
#' @param N0_N probability p = N0/N
#' @param N size of ranking list N
#' @details
#' Formula of F(p) is used to fit model with data via nlstools.
#' @author Marc Barthelemy and Julie Gravier

formula_phi_asymetric_exponential <- as.formula(Fresult ~ ps * ((1-N0_N) * exp(-(1-N0_N)/(ro/N)) + N0_N*( 1- exp(-(1-N0_N)/(ro/N)) ) ) 
                                                + (1-ps) * (pd/ro) * exp(-(1-N0_N)/(ro/N)) )


#' @title fit formula of F (from Ft)
#' @param N0_N probability p = N0/N
#' @param vectorank cumsum de N0 en 1 -> N0
#' @param N0 size of ranking list N0
#' @details
#' Formula is used to fit model with data via nlstools.
#' \eqn{Ft = \frac{ps(1-p)}{N0}\sum{r^-\alpha} + pd(1-\frac{ps}{N0}\sum{r^-\alpha})}
#' @author Marc Barthelemy and Julie Gravier
formula_F_asymetric <- as.formula(Ft ~ (ps*(1 - N0_N))/N0 * (vectorank^-alpha) + 
                                    pd*(1 - ps/N0 * (vectorank^-alpha)) )


#' @title fit formula of overlap decay
#' @param t_T time t/T
#' @details
#' Formula of F(o) is used to fit model with data via nlstools.
#' \eqn{F_0 = \exp(-a \times t/T)}
#' @author Marc Barthelemy and Julie Gravier

formula_overlapping <- as.formula(Overlapfit ~ exp(-a * t_T))



#### Iniguez et al. 2022 metrcis ####
#' @title compute F
#' @returns a tibble
#' @author adaptation of Iniguez et al. 2022 metrics (by Julie Gravier)
#' 
f_calculus_Finiguez <- function(vector_entry, times, list_entry, N, vec_not_continuous = FALSE){
  output_f <- tibble()
  for (i in 1:length(vector_entry)) {
    liste_vec <- vector()
    # Each times: identification if items cross the N0/N border normalized by size N0
    for (t in seq(2, length(list_entry), 1)) {
      # comparing list t and t-1
      extract_differences <- list_entry[[t]][1:i] %in% list_entry[[t-1]][1:i] #
      # count differences (when items == FALSE)
      n_f <- length(extract_differences[extract_differences == FALSE])/i
      liste_vec[t] <- n_f
    }
    # creation of output tibble
    output_f <- output_f %>%
      bind_rows(tibble(
        N0_N = if(vec_not_continuous == TRUE){vector_entry[i]/N} else {i/N},
        Fresult = sum(liste_vec, na.rm = TRUE)/(times-1),
        times = times
      ))
  }
  return(output_f)
}


#' @title compute Flux Ft
#' @returns a tibble
#' @author adaptation of Iniguez et al. 2022 metrics (by Julie Gravier)
#' 
f_calculus_Ft <- function(vector_entry, times, list_entry, N, vec_not_continuous = FALSE){
  output_f <- tibble()
  for (i in 1:length(vector_entry)) {
    liste_vec <- vector()
    # Each times: identification if items cross the N0/N border normalized by size N0
    for (t in seq(2, length(list_entry), 1)) {
      # comparing list t and t-1
      extract_differences <- list_entry[[t]][1:i] %in% list_entry[[t-1]][1:i] #
      # count differences (when items == FALSE)
      n_f <- length(extract_differences[extract_differences == FALSE])/i
      # creation of output tibble
      output_f <- output_f %>%
        bind_rows(tibble(
          N0_N = if(vec_not_continuous == TRUE){vector_entry[i]/N} else {i/N},
          Ft = n_f,
          times = t
        ))
    }
  }
  return(output_f)
}
