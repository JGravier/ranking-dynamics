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

#' @title calculus of F turnover
#' @param vector_entry the initial vector of items that will be re-rank
#' @param times the vector of N iteration, a seq(1,N,1)
#' @param list_entry the list of results of ranking model
#' @returns a tibble of two variables: N0/N and F
#' @author Julie Gravier

f_calculus_F <- function(vector_entry, times, list_entry){
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
        Fresult = sum(liste_vec, na.rm = TRUE)/length(times)
      ))
  }
  return(output_f)
}



