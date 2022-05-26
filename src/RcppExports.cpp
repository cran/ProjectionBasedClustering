// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// DijkstraSSSP
NumericVector DijkstraSSSP(NumericMatrix Adj, NumericMatrix Costs, int source);
RcppExport SEXP _ProjectionBasedClustering_DijkstraSSSP(SEXP AdjSEXP, SEXP CostsSEXP, SEXP sourceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type Adj(AdjSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type Costs(CostsSEXP);
    Rcpp::traits::input_parameter< int >::type source(sourceSEXP);
    rcpp_result_gen = Rcpp::wrap(DijkstraSSSP(Adj, Costs, source));
    return rcpp_result_gen;
END_RCPP
}
// c_NeRV
NumericMatrix c_NeRV(NumericMatrix data, double lambda, int lastNeighbor, int iterations, int stepsPerRound, int stepsOnLastRound, bool randominit, int outputDimension, Function pca);
RcppExport SEXP _ProjectionBasedClustering_c_NeRV(SEXP dataSEXP, SEXP lambdaSEXP, SEXP lastNeighborSEXP, SEXP iterationsSEXP, SEXP stepsPerRoundSEXP, SEXP stepsOnLastRoundSEXP, SEXP randominitSEXP, SEXP outputDimensionSEXP, SEXP pcaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< int >::type lastNeighbor(lastNeighborSEXP);
    Rcpp::traits::input_parameter< int >::type iterations(iterationsSEXP);
    Rcpp::traits::input_parameter< int >::type stepsPerRound(stepsPerRoundSEXP);
    Rcpp::traits::input_parameter< int >::type stepsOnLastRound(stepsOnLastRoundSEXP);
    Rcpp::traits::input_parameter< bool >::type randominit(randominitSEXP);
    Rcpp::traits::input_parameter< int >::type outputDimension(outputDimensionSEXP);
    Rcpp::traits::input_parameter< Function >::type pca(pcaSEXP);
    rcpp_result_gen = Rcpp::wrap(c_NeRV(data, lambda, lastNeighbor, iterations, stepsPerRound, stepsOnLastRound, randominit, outputDimension, pca));
    return rcpp_result_gen;
END_RCPP
}
// c_measure
NumericMatrix c_measure(NumericMatrix datamat, NumericMatrix projmat, unsigned int lastNeighbor);
RcppExport SEXP _ProjectionBasedClustering_c_measure(SEXP datamatSEXP, SEXP projmatSEXP, SEXP lastNeighborSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type datamat(datamatSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type projmat(projmatSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type lastNeighbor(lastNeighborSEXP);
    rcpp_result_gen = Rcpp::wrap(c_measure(datamat, projmat, lastNeighbor));
    return rcpp_result_gen;
END_RCPP
}
// c_klmeasure
List c_klmeasure(NumericMatrix Data, NumericMatrix pData, int NeighborhoodSize);
RcppExport SEXP _ProjectionBasedClustering_c_klmeasure(SEXP DataSEXP, SEXP pDataSEXP, SEXP NeighborhoodSizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type Data(DataSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type pData(pDataSEXP);
    Rcpp::traits::input_parameter< int >::type NeighborhoodSize(NeighborhoodSizeSEXP);
    rcpp_result_gen = Rcpp::wrap(c_klmeasure(Data, pData, NeighborhoodSize));
    return rcpp_result_gen;
END_RCPP
}
// klrank
List klrank(NumericMatrix Data, NumericMatrix pData, int NeighborhoodSize);
RcppExport SEXP _ProjectionBasedClustering_klrank(SEXP DataSEXP, SEXP pDataSEXP, SEXP NeighborhoodSizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type Data(DataSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type pData(pDataSEXP);
    Rcpp::traits::input_parameter< int >::type NeighborhoodSize(NeighborhoodSizeSEXP);
    rcpp_result_gen = Rcpp::wrap(klrank(Data, pData, NeighborhoodSize));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ProjectionBasedClustering_DijkstraSSSP", (DL_FUNC) &_ProjectionBasedClustering_DijkstraSSSP, 3},
    {"_ProjectionBasedClustering_c_NeRV", (DL_FUNC) &_ProjectionBasedClustering_c_NeRV, 9},
    {"_ProjectionBasedClustering_c_measure", (DL_FUNC) &_ProjectionBasedClustering_c_measure, 3},
    {"_ProjectionBasedClustering_c_klmeasure", (DL_FUNC) &_ProjectionBasedClustering_c_klmeasure, 3},
    {"_ProjectionBasedClustering_klrank", (DL_FUNC) &_ProjectionBasedClustering_klrank, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_ProjectionBasedClustering(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
