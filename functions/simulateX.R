simulateX <-
function(object, nsim = 1, seed = NULL, X, ...) {
        object$fitted.values <- predict(object, X)
        pull(simulate(object = object, nsim = nsim, seed = seed, ...))
}
