#' Partition a sequence into blocks
block_seq <- function(dim_param, block_num, block_size) {
  if (!missing(block_num) & !missing(block_size))
    stop("'block_num' and 'block_size' cannot both be specified.")
  if (!missing(block_num))
    return( block_seq_by_num(dim_param, block_num) )
  if (!missing(block_size))
    return( block_seq_by_size(dim_param, block_size) )
}
#' [Core] Partition a sequence into blocks given number of blocks
block_seq_by_num <- function(dim_param, block_num) {
  res <- unique(floor(seq(1, dim_param + 1, length.out = block_num)))
  res
}
#' [Core] Partition a sequence into blocks given block size
block_seq_by_size <- function(dim_param, block_size) {
  res <- seq(1, dim_param + 1, by = block_size)
  if (tail(res, 1) != dim_param + 1) res <- c(res, dim_param + 1)
  res
}


add_one <- function(x) {
  x+1
}
