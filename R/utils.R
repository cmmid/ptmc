extract_long_post <- function(mcmc_out, ...) {
    mcmc_out %>% combine %>% as.data.frame %>%
        dplyr::select(...) %>% 
        pivot_longer(everything(), names_to = "var", values_to = "value")
}


extract_post <- function(mcmc_out, ...) {
    mcmc_out %>% combine %>% as.data.frame %>%
        dplyr::select(...) 
}

make_long_discrete <- function(matrix_list) {
  matrix_list <- post$discrete
  # Initialize an empty list to store data.frames for each matrix
  combined_df <- map_df(seq_along(matrix_list), 
    function(mat_index) {
    # Extract the matrix
    mat <- matrix_list[[mat_index]] 
    colnames(mat) <- 1:ncol(mat)
    rownames(mat) <- 1:nrow(mat)
    mat %>% as.data.frame %>% mutate(sample_no = 1:nrow(mat)) %>% pivot_longer(!sample_no, 
      names_to = "idx", values_to = "value") %>% mutate(chain = mat_index)
    }
  )
  combined_df
}