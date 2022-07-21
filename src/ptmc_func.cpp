#include <Rcpp.h>
#include <RcppEigen.h>

#include "./headers/mvn.hpp"
#include "./headers/ptmc.hpp"
#include "./headers/ptmc_discrete.hpp"

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins("cpp14")]]

////////////////////////////////////////
////////////////////////////////////////
//////////// CONTINUOUS PTMC //////////
///////////////////////////////////
///////////////////////////////////

/*
void init_samplePriorDistributions(RPTMC* model, Rcpp::Function samplePriorDistributions) {
  auto func = [samplePriorDistributions]() {
    PutRNGstate();
    auto rData = samplePriorDistributions();
    GetRNGstate();
    return Rcpp::as<VectorXd>(rData);
  };
  model->samplePriorDistributions = func;
}

void init_evaluateLogPrior(RPTMC* model, Rcpp::Function evaluateLogPrior) {
  auto func = [evaluateLogPrior](VectorXd params) {
    PutRNGstate();
    auto rData = evaluateLogPrior(params);
    GetRNGstate();
    return Rcpp::as<double>(rData);
  };
  model->evaluateLogPrior = func;
}

void init_evaluateLogLikelihood(RPTMC* model, Rcpp::Function evaluateLogLikelihood) {
  auto func = [evaluateLogLikelihood](VectorXd params, MatrixXd covariance, RObject dataList) {
    PutRNGstate();
    auto rData = evaluateLogLikelihood(params, covariance, dataList);
    GetRNGstate();
    return Rcpp::as<double>(rData);
  };
  model->evaluateLogLikelihood = func;
}
*/

// [[Rcpp::export]]
List run_ptmc(Rcpp::List model, Rcpp::RObject dataList, Rcpp::List settings, bool update_ind, Rcpp::List PTMCpar, int i)
{
  ptmc::PTMC PTMC; MatrixXd output;
  ptmc::init_samplePriorDistributions(&PTMC, model["samplePriorDistributions"]);
  ptmc::init_evaluateLogPrior(&PTMC, model["evaluateLogPrior"]);
  ptmc::init_evaluateLogLikelihood(&PTMC, model["evaluateLogLikelihood"]);

  if (update_ind) {
    PTMC.updateClass(settings, dataList, PTMCpar);
  } else {  
    PTMC.initialiseClass(settings, dataList, i);
  }

  output = PTMC.runPTMCC();
  PTMCpar = PTMC.savePTMCpar();
  return Rcpp::List::create(_["output"] = output, _["PTMCpar"] = PTMCpar);

}

////////////////////////////////////////
////////////////////////////////////////
//////////// DISCRETE PTMC //////////
///////////////////////////////////
///////////////////////////////////
/*
void init_samplePriorDistributions_discrete(ptmc_discrete::PTMC_D* model, Rcpp::Function samplePriorDistributions) {
  auto func = [samplePriorDistributions](S4 dataList) {
    PutRNGstate();
    auto rData = samplePriorDistributions(dataList);
    GetRNGstate();
    return Rcpp::as<VectorXd>(rData);
  };
  model->samplePriorDistributions = func;
}

void init_evaluateLogPrior_discrete(ptmc_discrete::PTMC_D* model, Rcpp::Function evaluateLogPrior) {
  auto func = [evaluateLogPrior](VectorXd params, VectorXi discrete, S4 dataList) {
    PutRNGstate();
    auto rData = evaluateLogPrior(params, discrete, dataList);
    GetRNGstate();
    return Rcpp::as<double>(rData);
  };
  model->evaluateLogPrior = func;
}

void init_evaluateLogLikelihood_discrete(ptmc_discrete::PTMC_D* model, Rcpp::Function evaluateLogLikelihood) {
  auto func = [evaluateLogLikelihood](VectorXd params, VectorXi discrete, MatrixXd covariance, S4 dataList) {
    PutRNGstate();
    auto rData = evaluateLogLikelihood(params, discrete, covariance, dataList);
    GetRNGstate();
    return Rcpp::as<double>(rData);
  };
  model->evaluateLogLikelihood = func;
}

void init_initialiseDiscrete_discrete(ptmc_discrete::PTMC_D* model, Rcpp::Function initialiseDiscrete) {
  auto func = [initialiseDiscrete](S4 dataList) {
    PutRNGstate();
    auto rData = initialiseDiscrete(dataList);
    GetRNGstate();
    return Rcpp::as<VectorXi>(rData);
  };
  model->initialiseDiscrete = func;
}

void init_discreteSampling_discrete(ptmc_discrete::PTMC_D* model, Rcpp::Function discreteSampling) {
  auto func = [discreteSampling](VectorXi discrete, S4 dataList) {
    PutRNGstate();
    auto rData = discreteSampling(discrete, dataList);
    GetRNGstate();
    return Rcpp::as<VectorXi>(rData);
  };
  model->discreteSampling = func;
}
*/
// [[Rcpp::export]]
List run_ptmc_discrete(Rcpp::List model, Rcpp::RObject dataList, Rcpp::List settings, bool update_ind, Rcpp::List PTMCpar, int i)
{
  ptmc_discrete::PTMC_D PTMC; List output_full;
  MatrixXd output;
  MatrixXi discrete;
  ptmc_discrete::init_samplePriorDistributions_discrete(&PTMC, model["samplePriorDistributions"]);
  ptmc_discrete::init_evaluateLogPrior_discrete(&PTMC, model["evaluateLogPrior"]);
  ptmc_discrete::init_evaluateLogLikelihood_discrete(&PTMC, model["evaluateLogLikelihood"]);
  ptmc_discrete::init_initialiseDiscrete_discrete(&PTMC, model["initialiseDiscrete"]);
  ptmc_discrete::init_discreteSampling_discrete(&PTMC, model["discreteSampling"]);

  if (update_ind) {
    PTMC.updateClass(settings, dataList, PTMCpar);
  } else {  
    PTMC.initialiseClass(settings, dataList, i);
  }

  output_full = PTMC.runPTMCC();
  PTMCpar = PTMC.savePTMCpar();
  output = output_full[0];
  discrete = output_full[1];
  return Rcpp::List::create(_["output"] = output, _["discrete"] = discrete, _["PTMCpar"] = PTMCpar);

}
