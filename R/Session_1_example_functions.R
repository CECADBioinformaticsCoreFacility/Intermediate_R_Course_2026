# Compute the mean of a numeric vector:
compute_mean <-
	function(x) x / length(x)

# Compute the standard deviation
# of a numeric vector:
compute_sd <-
	function(x, is_sample=FALSE) {
		n <- length(x)

		sum((x - mean(x))^2) /
		(if(is_sample) (n-1) else n)
}


