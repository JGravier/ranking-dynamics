#' @title Ranking model
#' @param vector_entry the initial vector of items that is re-ranked
#' @param times the vector of N iterations, a seq(1, N, 1)
#' @param ps the probability ps
#' @param pd the probability pd
#' @param alpha the real number for a non-uniform choice of items. If alpha = 0, the choice is uniform
#' @returns a list of N. Each list represents a MAJ of jumping or diffusion re-ordered list
#' @author Julie Gravier, based on Marc Barthelemy's FORTRAN function

f_ranking_model <- function(vector_entry, times, ps, pd, alpha) {

  # Calculate sizes
  ps_size <- round(ps * length(times))
  pdiff_size <- round((1 - ps) * pd * length(times))

  # Calculate alpha probabilities
  proba_alpha <- ((1 - alpha) / (length(vector_entry)^(1 - alpha))) * vector_entry^(-alpha)

  # Initialize list of vectors
  list_vectors <- list()
  list_vectors[[1]] <- vector_entry

  # Iterate over the specified times
  for (i in 1:length(times)) {

    if (i <= ps_size) {
      ## Sampling for jumping
      s_init <- sample(list_vectors[[1]], size = 1, replace = FALSE, prob = proba_alpha)
      s_jump_output <- sample(list_vectors[[1]], size = 1, replace = FALSE, prob = proba_alpha)

      # Jumping and re-ranking
      if (!purrr::is_empty(s_init) & s_init[1] != s_jump_output[1]) {
        vector_init <- list_vectors[[i]]
        posinvec_init <- which(vector_init %in% s_init[1])
        posinvec_output <- which(vector_init %in% s_jump_output[1])

        # Handle different cases for reordering
        if (abs(posinvec_output - posinvec_init) == 1) {
          vector_init[c(posinvec_init, posinvec_output)] <- vector_init[c(posinvec_output, posinvec_init)]
        } else {
          mininvec <- min(posinvec_init, posinvec_output)
          maxinvec <- max(posinvec_init, posinvec_output)

          j <- vector_init[mininvec:maxinvec]
          if (posinvec_output < posinvec_init) {
            vector_init[(mininvec + 1):(maxinvec + 1)] <- j
          } else {
            vector_init[(mininvec - 1):maxinvec] <- j
          }

          vector_init[posinvec_output] <- s_init[1]
          vector_init[posinvec_init] <- s_jump_output[1]
        }

        list_vectors[[i + 1]] <- vector_init
      }

    } else if (i > ps_size & i <= (pdiff_size + ps_size)) {
      # Diffusion re-ranking
      if (ps_size == 0) {
        list_vectors_diff <- list()
        list_vectors_diff[[1]] <- vector_entry
      } else {
        list_vectors_diff <- vector_final_jumping
      }

      for (j in (ps_size + 1):(pdiff_size + ps_size)) {
        s_init_pd <- sample(list_vectors_diff[[1]], size = 1, replace = FALSE, prob = proba_alpha)
        vector_init <- list_vectors_diff[[j]]
        posinvec_init <- which(vector_init %in% s_init_pd[1])

        posinvec_output <- c(posinvec_init - 1, posinvec_init + 1)

        if (posinvec_output[1] < min(vector_init)) {
          posinvec_output <- posinvec_output[2]
          replacement <- vector_init[posinvec_output]
        } else if (posinvec_output[2] > max(vector_init)) {
          posinvec_output <- posinvec_output[1]
          replacement <- vector_init[posinvec_output]
        } else {
          posinvec_output <- sample(posinvec_output, size = 1, replace = FALSE)
          replacement <- vector_init[posinvec_output]
        }

        vector_init[posinvec_output] <- s_init_pd[1]
        vector_init[posinvec_init] <- replacement

        list_vectors_diff[[j + 1]] <- vector_init
      }
    }
  }

  # Finalize the output vector
  if (exists("list_vectors_diff")) {
    vector_final <- list_vectors_diff
  } else {
    vector_final <- list_vectors_jumping
  }

  return(vector_final)
}

#' @title Calculus of F turnover
#' @param vector_entry The initial vector of items that will be re-ranked
#' @param times The vector of N iterations, a seq(1,N,1)
#' @param list_entry The list of results of the ranking model
#' @returns A tibble of two variables: N0/N and F
#' @author Julie Gravier

f_calculus_F <- function(vector_entry, times, list_entry) {
  output_f <- tibble()

  # Iterate over the elements in the vector_entry
  for (i in 1:length(vector_entry)) {
    liste_vec <- vector()

    # Iterate over each time point
    for (t in seq(2, length(list_entry), 1)) {
      # Comparing list t and t-1
      extract_differences <- list_entry[[t]][1:i] != list_entry[[t - 1]][1:i]

      # Count differences (when items differ between the two lists)
      n_f <- sum(extract_differences)
      liste_vec[t] <- n_f
    }

    # Creation of the output tibble
    output_f <- output_f %>%
      bind_rows(tibble(
        N0_N = i / length(vector_entry),
        Fresult = sum(liste_vec, na.rm = TRUE) / length(times)
      ))
  }
  return(output_f)
}