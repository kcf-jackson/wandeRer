#' Stochastic Search
#' @description An stochasic algorithm for optimisation
#' @param dim_param integer; the dimension of the parameters.
#' @param perf_fun function; the performance function mapping parameters to an outcome,
#' e.g. a curve.
#' @param loss_fun function; the loss function of two outcomes.
#' @param target_perf function; the performance to be matched, e.g. a target data curve.
#' @param max_iter integer; maximum number of iterations.
#' @param tol numeric; tolerence for the loss functions.
#' @param curiosity numeric; search parameter.
#' @param block_num integer.
#' @param block_size integer.
#' @param lambda numeric; regularisation parameter.
#' @param reg_fun function; regularisation funciton, see 'zero_reg', 'L1_reg', 'L2_reg'.
#' @param param numeric vector; parameter from previous run of the function.
#' @details Denote the dimension of the parameters by d.
#' If the stochastic search didn't improve in 5*d block iterations, then it will
#' increase the curiosity by 10 folds. On the other hand, all improvements are
#' associated with a 0.001 decrease curiosity. This is to create oscillatary
#' behaviour of the curiosity parameter.
stochastic_search <- function(dim_param, perf_fun,
                              loss_fun=ls_loss, target_perf,
                              max_iter=100, tol=0, curiosity=1,
                              block_num, block_size,
                              lambda=0, reg_fun=zero_reg, param) {
  #initialisation
  curiosity_increase_factor <- 10
  curioisty_decrease_factor <- 0.999
  if (!missing(block_num) & !missing(block_size))
    stop("'block_num' and 'block_size' cannot both be specified.")
  if (missing(param)) param <- rep(0, dim_param)
  current_perf <- perf_fun(param)
  current_loss <- loss_fun(current_perf, target_perf) + lambda * reg_fun(param)
  cat("Beginning loss: ", current_loss, "\n")

  iter <- since_update_counter <- 0
  while ((iter < max_iter) & (current_loss > tol)) {
    if (since_update_counter %% (5 * dim_param) == 1) {
      curiosity <- curiosity * curiosity_increase_factor
    }
    iter <- iter + 1
    if (iter %% 10 == 0) print(iter)
    loop_seq <- block_seq(dim_param, block_num, block_size)
    for (i in head(seq_along(loop_seq), -1)) {
      since_update_counter %<>% add_one()
      new_param <- param
      update_range <- loop_seq[i]:(loop_seq[i+1] - 1)
      new_param[update_range] <- new_param[update_range] +
        curiosity * runif(length(update_range), min = -0.001, max = 0.001)
      new_perf <- tryCatch(perf_fun(new_param), error = function(e) e)
      if ("error" %in% class(new_perf)) next
      new_loss <- loss_fun(new_perf, target_perf) + lambda * reg_fun(param)
      if (new_loss < current_loss) {
        curiosity <- curiosity * curioisty_decrease_factor
        since_update_counter <- 0
        param <- new_param
        current_perf <- new_perf
        current_loss <- new_loss
        cat("Improved loss: ", current_loss, "\n")
      }
    }
    # cat("Current loss: ", current_loss, "\n")
  }
  cat("Final loss: ", current_loss, "\n")
  list(parameter = param, output = current_perf, loss = current_loss)
}
