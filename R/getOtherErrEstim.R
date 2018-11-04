getOtherErrEstim <-
function(observed, predicted) {
    N <- length(predicted)
    diffPO <- predicted - observed
    result <- c(sqrt(sum(diffPO^2)/sum(observed^2)), (1/N) * sum(abs(diffPO)), 
                (1/N) * sum(diffPO))
    names(result) <- c("relRMSE", "MAE", "BE")
    result
}
