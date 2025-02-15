#ifndef PTMC_D_HPP
#define PTMC_D_HPP

#include <iostream>
#include <fstream>
#include <iomanip>
#include <boost/random.hpp>
#include <boost/numeric/odeint.hpp>
#include <boost/math/distributions.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <vector>
#include <random>
#include <math.h>

#define EIGEN_DONT_VECTORIZE
#define EIGEN_DISABLE_UNALIGNED_ARRAY_ASSERT

using namespace Rcpp;
using namespace std;
using namespace Eigen;
using namespace boost::math;
// [[Rcpp::plugins("cpp14")]]

#define PI 3.14159265358979323846

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b)) // define MAX function for use later
#endif

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b)) // define MAX function for use later
#endif

namespace ptmc_discrete{
    struct PTMC_D
    {
        PTMC_D() {}
        
        bool conPropIn = true;
        bool disPropIn = false;

        VectorXd nonadaptiveScalar, adaptiveScalar, lowerParBounds, upperParBounds;
        MatrixXd nonadaptiveCovarianceMat, adaptiveCovarianceMat;
        MatrixXd currentSample, currentSampleMean;
        MatrixXd posteriorOut;
        MatrixXi posteriorDiscrete;
        MatrixXd currentCovarianceMatrix;
        VectorXd currentLogPosterior, proposalSample;
        VectorXd temperatureLadder, temperatureLadderParameterised;
        
        MatrixXi currentDiscrete;
        VectorXi proposalDiscrete;

        VectorXd iPosterior;
        int iterations, posteriorSamplesLength, thin, burninPosterior, burninAdaptiveCov, consoleUpdates, updatesAdaptiveCov, updatesAdaptiveTemp, chainNumber;
        double updateDiscreteFreq;
        int lengthDiscreteVec;
        int numberTempChains, numberFittedPar, numTempChainsNonAdaptive;
        int workingChainNumber, workingIteration;
        bool onDebug, onAdaptiveCov, onAdaptiveTemp;
        bool isSampleAccepted, isProposalAdaptive;

        RObject dataList;
        VectorXi counterFuncEval, counterAccepted, counterPosterior ,counterAdaptive;
        VectorXi counterNonAdaptive, counterFuncEvalTemp, counterAcceptTemp;
        
        double proposedLogPosterior, alpha, covarMaxVal, covarInitVal, covarInitValAdapt;

        std::function<VectorXd(RObject)> samplePriorDistributions;
        std::function<VectorXi(RObject)> initialiseDiscrete;
        std::function<double(VectorXd, VectorXi, RObject)> evaluateLogPrior;
        std::function<VectorXi(VectorXi, RObject)> discreteSampling;
        std::function<double(VectorXd, VectorXi, MatrixXd, RObject)> evaluateLogLikelihood;

        Mvn Mvn_sampler;


        double stepSizeRobbinsMonro;
        double evalLogPosterior(const VectorXd& param, const VectorXi& discrete, const MatrixXd& covariance, const RObject& dataList)
        {
            double logPrior = this->evaluateLogPrior(param, discrete, dataList);
            if (isinf(logPrior))
                return log(0);
          
            double logLikelihood = this->evaluateLogLikelihood(param, discrete, covariance, dataList);
            return logPrior + logLikelihood;
        }
        
        // "A handy approximation for the error function and its inverse" by Sergei Winitzki.
        double ErfInv(float x){
            double tt1, tt2, lnx, sgn;
            sgn = (x < 0) ? -1.0 : 1.0;
            
            x = (1 - x)*(1 + x);
            lnx = logf(x);
            
            double pi = atan(1)*4;    

            tt1 = 2/(pi*0.147) + 0.5 * lnx;
            tt2 = 1/(0.147) * lnx;
            
            return(sgn*sqrtf(-tt1 + sqrtf(tt1*tt1 - tt2)));
        }
        
        void initialiseClass(List settings, RObject dataList, int i)
        {
            this->dataList = dataList;
            this->chainNumber = i;
            this->numberTempChains = settings["numberTempChains"];
            this->numTempChainsNonAdaptive = this->numberTempChains / 2;

            this->numberFittedPar = settings["numberFittedPar"];
            this->iterations = settings["iterations"];
            this->thin = settings["thin"];
            this->burninPosterior = settings["burninPosterior"];
            this->burninAdaptiveCov = settings["burninAdaptiveCov"];
            this->consoleUpdates = settings["consoleUpdates"];
            this->onAdaptiveCov = settings["onAdaptiveCov"];
            this->onAdaptiveTemp = settings["onAdaptiveTemp"];
            this->updatesAdaptiveCov = settings["updatesAdaptiveCov"];
            this->updatesAdaptiveTemp = settings["updatesAdaptiveTemp"];
            this->onDebug = settings["onDebug"];
            this->lengthDiscreteVec = settings["lengthDiscreteVec"];
            
            this->lowerParBounds = settings["lowerParBounds"];
            this->upperParBounds = settings["upperParBounds"];

            this->covarInitVal = settings["covarInitVal"];
            this->covarInitValAdapt = settings["covarInitValAdapt"];
            this->covarMaxVal = settings["covarMaxVal"];

            this->updateDiscreteFreq = settings["updateDiscreteFreq"];

            this->counterFuncEval = VectorXi::Zero(this->numberTempChains);
            this->counterAccepted = VectorXi::Zero(this->numberTempChains);
            this->counterPosterior = VectorXi::Zero(this->numberTempChains);
            this->counterAdaptive = VectorXi::Zero(this->numberTempChains);
            this->counterNonAdaptive = VectorXi::Zero(this->numberTempChains);
            this->counterFuncEvalTemp = VectorXi::Zero(this->numberTempChains);
            this->counterAcceptTemp = VectorXi::Zero(this->numberTempChains);
            
            this->iPosterior = VectorXd::Zero(this->numberTempChains);

            this->posteriorSamplesLength = (this->iterations-this->burninPosterior)/(this->thin);
            this->posteriorOut = MatrixXd::Zero(this->posteriorSamplesLength, this->numberFittedPar + 3);
            this->posteriorDiscrete = MatrixXi::Zero(this->posteriorSamplesLength, this->lengthDiscreteVec);

            this->currentLogPosterior = VectorXd::Zero(this->numberTempChains);
            this->currentSampleMean = MatrixXd::Zero(this->numberTempChains,this->numberFittedPar);
            this->currentSample = MatrixXd::Zero(this->numberTempChains,this->numberFittedPar);
            this->currentDiscrete = MatrixXi::Zero(this->numberTempChains, this->lengthDiscreteVec);

            this->proposalSample = VectorXd::Zero(this->numberFittedPar);
            this->proposalDiscrete = VectorXi::Zero(this->lengthDiscreteVec);

            this->nonadaptiveScalar = VectorXd::Zero(this->numberTempChains);
            this->adaptiveScalar = VectorXd::Zero(this->numberTempChains);
            
            this->currentCovarianceMatrix = MatrixXd::Zero(this->numberFittedPar, this->numberFittedPar );
            this->nonadaptiveCovarianceMat = MatrixXd::Zero(this->numberFittedPar, this->numberFittedPar );
            this->adaptiveCovarianceMat = MatrixXd::Zero(this->numberTempChains * this->numberFittedPar ,this->numberFittedPar );
            this->temperatureLadder =  VectorXd::Zero(this->numberTempChains);
            this->temperatureLadderParameterised =  VectorXd::Zero(this->numberTempChains-1);
            

            VectorXd initialSample;
            VectorXi initialDiscrete;
            double initialLogLikelihood;
            MatrixXd initialCovarianceMatrix;
            
            for(int parNum = 0; parNum < this->numberFittedPar ; parNum++){
                this->nonadaptiveCovarianceMat(parNum,parNum) = this->covarInitVal*(this->upperParBounds(parNum) - this->lowerParBounds(parNum));
                for (int chainNum = 0; chainNum < this->numberTempChains; chainNum++){
                    this->adaptiveCovarianceMat(chainNum*this->numberFittedPar+parNum,parNum) = this->covarInitValAdapt*(this->upperParBounds(parNum) - this->lowerParBounds(parNum));
                }
            }
            

            for (int chainNum = 0; chainNum < this->numberTempChains; chainNum++){

                this->nonadaptiveScalar(chainNum) = log(0.1*0.1/(double)this->numberFittedPar);
                this->adaptiveScalar(chainNum) = log(2.382*2.382/(double)this->numberFittedPar);
                
                temperatureLadder[chainNum] = pow(10, 7.0*(chainNum)/(numberTempChains-1.0));
                if (chainNum > 0)
                    temperatureLadderParameterised[chainNum-1] = log(temperatureLadder[chainNum]-temperatureLadder[chainNum-1]);
                
                initialSample = this->samplePriorDistributions(this->dataList);

                initialDiscrete = this->initialiseDiscrete(this->dataList);
                this->currentCovarianceMatrix = this->nonadaptiveScalar(chainNum)*this->nonadaptiveCovarianceMat;

                initialLogLikelihood = this->evalLogPosterior(initialSample, initialDiscrete, this->currentCovarianceMatrix, this->dataList);
                while(isinf(initialLogLikelihood) || isnan(initialLogLikelihood)){
                    initialSample = this->samplePriorDistributions(this->dataList);
                    initialDiscrete = this->initialiseDiscrete(this->dataList);
                    initialLogLikelihood = this->evalLogPosterior(initialSample, initialDiscrete, this->currentCovarianceMatrix, this->dataList);
                }

                this->currentSample.row(chainNum) = initialSample;
                this->currentSampleMean.row(chainNum) = initialSample;
                this->currentDiscrete.row(chainNum) = initialDiscrete;
                this->currentLogPosterior(chainNum) = initialLogLikelihood;
            }
            
            double alphaMVN = -sqrt(2)*ErfInv(0.234-1);
            this->stepSizeRobbinsMonro = (1.0-1.0/(double)this->numberFittedPar)*(pow(2*3.141, 0.5)*exp(alphaMVN*alphaMVN*0.5))/(2*alphaMVN) + 1.0/(this->numberFittedPar*0.234*(1-0.234));
        }

        void updateClass(List settings, RObject dataList, List PTMCpar)
        {
            this->dataList = dataList;

            this->numberTempChains = settings["numberTempChains"];
            this->numTempChainsNonAdaptive = this->numberTempChains / 2;

            this->numberFittedPar = settings["numberFittedPar"];
            this->iterations = settings["iterations"];
            this->thin = settings["thin"];
            this->burninPosterior = settings["burninPosterior"];
            this->burninAdaptiveCov = settings["burninAdaptiveCov"];
            this->consoleUpdates = settings["consoleUpdates"];
            this->onAdaptiveCov = settings["onAdaptiveCov"];
            this->onAdaptiveTemp = settings["onAdaptiveTemp"];
            this->updatesAdaptiveCov = settings["updatesAdaptiveCov"];
            this->updatesAdaptiveTemp = settings["updatesAdaptiveTemp"];
            this->onDebug = settings["onDebug"];
            
            this->lowerParBounds = settings["lowerParBounds"];
            this->upperParBounds = settings["upperParBounds"];

            // Counters 
            this->counterFuncEval = PTMCpar["counterFuncEval"];
            this->counterAccepted = PTMCpar["counterAccepted"];
            this->counterPosterior = PTMCpar["counterPosterior"];
            this->counterAdaptive = PTMCpar["counterAdaptive"];
            this->counterNonAdaptive = PTMCpar["counterNonAdaptive"];
            this->counterFuncEvalTemp = PTMCpar["counterFuncEvalTemp"];
            this->counterAcceptTemp = PTMCpar["counterAcceptTemp"];
            
            this->posteriorSamplesLength = (int)PTMCpar["posteriorSamplesLength"] + (this->iterations-this->burninPosterior)/(this->thin);
            
            this->posteriorOut = MatrixXd::Zero(this->posteriorSamplesLength, this->numberFittedPar+3);
            this->posteriorDiscrete = MatrixXi::Zero(this->posteriorSamplesLength, this->lengthDiscreteVec);

            MatrixXd posteriorOutPrev = PTMCpar["posteriorOut"];
            for (int i = 0; i < (int)PTMCpar["posteriorSamplesLength"]; i++)
                this->posteriorOut.row(i) = posteriorOutPrev.row(i);

            this->currentLogPosterior = PTMCpar["currentLogPosterior"];
            this->currentSampleMean = PTMCpar["currentSampleMean"];
            this->currentSample = PTMCpar["currentSample"];
            
            this->proposalSample = PTMCpar["proposalSample"];
            this->nonadaptiveScalar = PTMCpar["nonadaptiveScalar"];
            this->adaptiveScalar = PTMCpar["adaptiveScalar"];
            
            this->currentCovarianceMatrix = PTMCpar["currentCovarianceMatrix"];
            this->nonadaptiveCovarianceMat =PTMCpar["nonadaptiveCovarianceMat"];
            this->adaptiveCovarianceMat = PTMCpar["adaptiveCovarianceMat"];
            this->temperatureLadder =  PTMCpar["temperatureLadder"];
            this->temperatureLadderParameterised = PTMCpar["temperatureLadderParameterised"];
            
            double alphaMVN = -sqrt(2)*ErfInv(0.234-1);
            this->stepSizeRobbinsMonro = (1.0-1.0/(double)this->numberFittedPar)*(pow(2*3.141, 0.5)*exp(alphaMVN*alphaMVN*0.5))/(2*alphaMVN) + 1.0/(this->numberFittedPar*0.234*(1-0.234));
        }
        
        List savePTMCpar() 
        {
            List PTMCpar = 
                Rcpp::List::create(
                    _["counterFuncEval"] = this->counterFuncEval,
                    _["counterAccepted"] = this->counterAccepted,
                    _["counterPosterior"] = this->counterPosterior,
                    _["counterAdaptive"] = this->counterAdaptive,
                    _["counterNonAdaptive"] = this->counterNonAdaptive,
                    _["counterFuncEvalTemp"] = this->counterFuncEvalTemp,
                    _["counterAcceptTemp"] = this->counterAcceptTemp,

                    _["currentLogPosterior"] = this->currentLogPosterior,
                    _["currentSampleMean"] = this->currentSampleMean,
                    _["currentSample"] = this->currentSample,

                    _["proposalSample"] = this->proposalSample,
                    _["nonadaptiveScalar"] = this->nonadaptiveScalar,
                    _["adaptiveScalar"] = this->adaptiveScalar,

                    _["currentCovarianceMatrix"] = this->currentCovarianceMatrix,
                    _["nonadaptiveCovarianceMat"] = this->nonadaptiveCovarianceMat,
                    _["adaptiveCovarianceMat"] = this->adaptiveCovarianceMat,
                    _["temperatureLadder"] = this->temperatureLadder,
                    _["temperatureLadderParameterised"] = this->temperatureLadderParameterised,

                    _["posteriorSamplesLength"] = this->posteriorSamplesLength,
                    _["posteriorOut"] = this->posteriorOut
            );
            return PTMCpar;
        }
        
        List runPTMCC()
        {
            for (int i = 0; i < this->iterations; i++){
                this->workingIteration = i;
                updateAllChainsAndTemp();
            }
            List out = Rcpp::List::create(
                _["pars"] = this->posteriorOut.block(0, 0, this->posteriorSamplesLength, this->numberFittedPar+3),
                _["discrete"] = this->posteriorDiscrete.block(0, 0, this->posteriorSamplesLength, this->lengthDiscreteVec)
            );
            return out;
        }
        
        void updateAllChainsAndTemp()
        {
            for (int n = 0; n < numberTempChains; n++){
                this->workingChainNumber = n;
                if (onDebug) Rcpp::Rcout << "Pre: getAcceptanceRate" << std::endl;
                getAcceptanceRate();
                if (onDebug) Rcpp::Rcout << "Pre: updateSampleAndLogPosterior" << std::endl;
                updateSampleAndLogPosterior();
                if (onDebug) Rcpp::Rcout << "Pre: updateOutputPosterior" << std::endl;
                updateOutputPosterior();
                if (onDebug) Rcpp::Rcout << "Pre: updateProposal" << std::endl;
                if(this->conPropIn) {
                    updateProposal();
                }
            }
            if (onDebug) Rcpp::Rcout << "Pre: swapTemperatureChains" << std::endl;
            swapTemperatureChains();
            consoleUpdatefunction();
        }

        void getAcceptanceRate()
        {
            this->isSampleAccepted = false;
            if (this->updateDiscreteFreq == 0) {
                selectProposalDist();
                DiscreteProposalDist();
                this->proposedLogPosterior = this->evalLogPosterior(this->proposalSample, this->proposalDiscrete, this->currentCovarianceMatrix, this->dataList);
            } else {
                double p1 = uniformContinuousDist(0, 1);
                if (p1 < (1.0 / (1.0 + (double)this->updateDiscreteFreq))) {
                    this->conPropIn = true; this->disPropIn = false;
                    selectProposalDist();
                    this->proposedLogPosterior = this->evalLogPosterior(this->proposalSample, this->proposalDiscrete, this->currentCovarianceMatrix, this->dataList);
                }
                else {
                    this->conPropIn = false; this->disPropIn = true;
                    DiscreteProposalDist();
                    this->proposedLogPosterior = this->evalLogPosterior(this->proposalSample, this->proposalDiscrete, this->currentCovarianceMatrix, this->dataList);
                }
            }
            evaluateMetropolisRatio();
            this->counterFuncEval[this->workingChainNumber]++;
        }

        void DiscreteProposalDist() {
            this->proposalDiscrete = this->discreteSampling(this->currentDiscrete.row(this->workingChainNumber), this->dataList);
        }
        
        void selectProposalDist(){
            if (this->workingIteration < this->burninAdaptiveCov || uniformContinuousDist(0, 1) < 0.05 || !this->onAdaptiveCov || this->workingChainNumber > this->numTempChainsNonAdaptive ){
                generateSampleFromNonAdaptiveProposalDist();
            }
            else{
                generateSampleFromAdaptiveProposalDist();
            }
        }
        
        void generateSampleFromNonAdaptiveProposalDist()
        {
            double s;
          //  trimNonAdaptiveValues(this->nonadaptiveScalar[this->workingChainNumber]);
            s = exp(this->nonadaptiveScalar[this->workingChainNumber]);
            this->counterNonAdaptive[this->workingChainNumber]++; this->isProposalAdaptive = false;
            this->currentCovarianceMatrix = s*this->nonadaptiveCovarianceMat;
            Mvn_sampler.updateCholesky(this->currentSample.row(this->workingChainNumber).transpose(), this->currentCovarianceMatrix);
            this->proposalSample = Mvn_sampler.sampleTrunc(this->lowerParBounds, this->upperParBounds, 10, this->onDebug);
            
            errorCheckVectorValid(this->proposalSample);
        }
        
        void generateSampleFromAdaptiveProposalDist()
        {
            double s;
            s = exp(this->adaptiveScalar[this->workingChainNumber]);
            this->counterAdaptive[this->workingChainNumber]++; this->isProposalAdaptive = true;
            this->currentCovarianceMatrix = s*this->adaptiveCovarianceMat.block(this->workingChainNumber*this->numberFittedPar, 0, this->numberFittedPar, this->numberFittedPar);
            Mvn_sampler.updateCholesky(this->currentSample.row(this->workingChainNumber).transpose(), this->currentCovarianceMatrix);
            this->proposalSample = Mvn_sampler.sampleTrunc(this->lowerParBounds, this->upperParBounds, 10, this->onDebug);
            
            errorCheckVectorValid(this->proposalSample);
        }
        
        void evaluateMetropolisRatio()
        {
            if(std::isnan(this->proposedLogPosterior) || std::isinf(this->proposedLogPosterior))
                this->alpha = 0;
            else
                this->alpha = MIN(1.0, exp((this->proposedLogPosterior - this->currentLogPosterior[this->workingChainNumber])/this->temperatureLadder[this->workingChainNumber]));
        }
        
        void updateSampleAndLogPosterior()
        {
            if (uniformContinuousDist(0, 1) < this->alpha) {
                this->isSampleAccepted = true; this->counterAccepted[this->workingChainNumber]++;
                this->currentSample.row(this->workingChainNumber) = proposalSample;
                this->currentDiscrete.row(this->workingChainNumber) = proposalDiscrete;
                this->currentLogPosterior(this->workingChainNumber) = proposedLogPosterior;
            }
        }
        
        void updateOutputPosterior()
        {
            if ((this->workingIteration > (this->burninPosterior-1)) && 
                (this->workingIteration%thin == 0) &&
                (this->workingChainNumber == 0)) {
                int m = 0;

              //  int l = this->posteriorSamplesLength;
                for (int p = 0; p < this->numberFittedPar; p++)
                    this->posteriorOut(this->counterPosterior[m], p) = this->currentSample(m, p);
                
                this->posteriorOut(this->counterPosterior[m], this->numberFittedPar) = this->currentLogPosterior(m);
                this->posteriorOut(this->counterPosterior[m], this->numberFittedPar+1) = this->temperatureLadder[m];
                this->posteriorOut(this->counterPosterior[m], this->numberFittedPar+2) = (double)this->counterAccepted[m]/(double)this->counterFuncEval[m];

                for (int j = 0; j < this->lengthDiscreteVec; j++)
                    this->posteriorDiscrete(this->counterPosterior[m], j) = this->currentDiscrete(m, j);

                this->counterPosterior[m]++;
            }
        }
        
        void updateProposal()
        {
            int m = this->workingChainNumber;
            int P = this->numberFittedPar;
            // Update adaptive proposal stuff
            if (this->isProposalAdaptive){
                this->adaptiveScalar[m] += this->stepSizeRobbinsMonro * pow(1+this->counterAdaptive[m],-0.5)*(this->alpha - 0.234);
                errorCheckNumberValid(this->adaptiveScalar[m]);
                trimAdaptiveValues(this->adaptiveScalar[m]);
            }
            else{
                this->nonadaptiveScalar[m] += this->stepSizeRobbinsMonro * pow(1+this->counterNonAdaptive[m],-0.5)*(this->alpha - 0.234);
                errorCheckNumberValid(this->nonadaptiveScalar[m]);
                trimNonAdaptiveValues(this->nonadaptiveScalar[m]);
            }
        
            // Update adaptive proposal stuff
            if(((this->workingIteration) % (this->updatesAdaptiveCov) == 0) && (this->workingIteration > this->burninAdaptiveCov)){
                //int iPosterior = (this->workingIteration-this->burninAdaptiveCov);
                this->iPosterior[m]++;
                double gainFactor = pow(1+iPosterior[m], -0.5);
               // if (iPosterior == this->updatesAdaptiveCov){
                if (iPosterior[m] == 1){
                    this->currentSampleMean.row(m) = this->currentSample.row(m);
                    this->adaptiveScalar[m] = this->nonadaptiveScalar[m];
                }
                else{
                    this->currentSampleMean.row(m) = this->currentSampleMean.row(m) + gainFactor*(this->currentSample.row(m)-this->currentSampleMean.row(m));
                    errorCheckVectorValid(this->currentSampleMean.row(m).transpose());
                    this->adaptiveCovarianceMat.block(m*P,0,P,P) = this->adaptiveCovarianceMat.block(m*P,0,P,P) + gainFactor*((this->currentSample.row(m)-this->currentSampleMean.row(m)).transpose()*((this->currentSample.row(m))-(this->currentSampleMean.row(m)))) - gainFactor*this->adaptiveCovarianceMat.block(m*P,0,P,P);
                    errorCheckMatrixValid(this->adaptiveCovarianceMat.block(m*P,0,P,P));
                }
            }
        }
        
        void swapTemperatureChains()
        {
            if ((this->workingIteration%updatesAdaptiveTemp == 0)){
                for (int m = 0; m < this->numberTempChains; m++){
                    int p = uniformDiscreteDist(0, this->numberTempChains - 2);
                    int q = p+1;
                    
                    this->counterFuncEvalTemp[p] ++;
                    int alphaTemp = evaluateMetropolisRatioTemp(p, q);
                    if (uniformContinuousDist(0, 1) < alphaTemp){
                        this->counterAcceptTemp[p]++;
                        swapSamplesandLogPosterior(p, q);
                    }
                    this->temperatureLadderParameterised[p] += pow((1+this->counterFuncEvalTemp[p]),(-0.5))*(alphaTemp - 0.234);

                    if (this->temperatureLadderParameterised[p] < -2)
                        this->temperatureLadderParameterised[p] = -2; // truncaton so that temperatures aren't too close
                    
                    errorCheckNumberValid(this->temperatureLadderParameterised[p]);
                }
                if (this->onAdaptiveTemp){
                    for (int m = 0; m < this->numberTempChains-1; m++){
                        changeTemperature(m);
                        errorCheckTempNumberValid(this->temperatureLadder[m+1]);
                    }
                }
            }
        }
        
        void changeTemperature(int m)
        {
            this->temperatureLadder[m+1] = this->temperatureLadder[m] + exp(this->temperatureLadderParameterised[m]);
        }

        void trimNonAdaptiveValues(double value)
        {
            this->nonadaptiveScalar[this->workingChainNumber] = log(MAX(MIN(exp(value),  this->covarMaxVal), 1e-25));
        }
        
        void trimAdaptiveValues(double value)
        {
            this->adaptiveScalar[this->workingChainNumber] = log(MAX(MIN(exp(value), this->covarMaxVal), 1e-25));
        }

        void errorCheckNumberValid(double value)
        {
            if (isinf(value)||isnan(value)){
                Rcout << "The number is not finite." << endl;
                stop("Value: ", value);
            }
        }
        
        void errorCheckTempNumberValid(double temp)
        {
            if (temp < 1 ||isinf(temp)||isnan(temp)){
                Rcout << "The temperature is not finite." << endl;
                stop("Value: ", temp);
            }
        }
        
        void errorCheckVectorValid(const VectorXd& proposalSample)
        {
            for (int i = 0; i < this->numberFittedPar; i++){
                if (isinf(proposalSample(i))||isnan(proposalSample(i))){
                    Rcout << "The proposed vector is not finite." << endl;
                    stop("Value: ", proposalSample(i));
                }
            }
        }
        
        void errorCheckMatrixValid(const MatrixXd& covarianceMatrix)
        {
            for (int i = 0; i < this->numberFittedPar; i++){
                for (int j = 0; j < this->numberFittedPar; j++){
                    if (isinf(covarianceMatrix(i,j)) || isnan(covarianceMatrix(i,j))){
                        Rcout << "The proposed matrix is not finite." << endl;
                        stop("Value: ", covarianceMatrix(i,j));
                    }
                }
            }
        }
        
        double evaluateMetropolisRatioTemp(int p, int q)
        {
            double alphaTemp;
            if(std::isnan(this->currentLogPosterior[q] - this->currentLogPosterior[p]))
                alphaTemp = 0;
            else
                alphaTemp = MIN(1.0, exp((this->currentLogPosterior[q] - this->currentLogPosterior[p])*(1.0/this->temperatureLadder[p]-1.0/this->temperatureLadder[q])));
            
            return alphaTemp;
        }
        
        void swapSamplesandLogPosterior(int p, int q)
        {
            VectorXd swapSampleInterProp, swapSampleInterCurr;
            VectorXi swapDiscreteInterProp, swapDiscreteInterCurr;

            double swapLogPosteriorInterCurr, swapLogPosteriorInterProp;
            
            swapLogPosteriorInterCurr = this->currentLogPosterior(p); swapLogPosteriorInterProp = this->currentLogPosterior(q);
            this->currentLogPosterior(p) = swapLogPosteriorInterProp; this->currentLogPosterior(q) = swapLogPosteriorInterCurr;
            swapSampleInterCurr = this->currentSample.row(p); swapSampleInterProp = this->currentSample.row(q);
            this->currentSample.row(p) = swapSampleInterProp; this->currentSample.row(q) = swapSampleInterCurr;
            
            swapDiscreteInterCurr = this->currentDiscrete.row(p); swapDiscreteInterProp = this->currentDiscrete.row(q);
            this->currentDiscrete.row(p) = swapDiscreteInterProp; this->currentDiscrete.row(q) = swapDiscreteInterCurr;

        }
                
        double uniformContinuousDist(double minValue, double maxValue)
        {
            boost::random::uniform_real_distribution<> u(minValue,maxValue); return u(rng);
        }
        
        double uniformDiscreteDist(int minValue, int maxValue)
        {
            boost::random::uniform_int_distribution<> u(minValue, maxValue); return u(rng);
        }
            
        void consoleUpdatefunction()
        {
            int i = this->workingIteration;
            if(i%this->consoleUpdates == 0) {
                Rcpp::Rcout << "Running MCMC-PT iteration number: " << this->workingIteration << " of " <<  this->iterations << ". Chain number " << this->chainNumber << ". Current logpost: " << this->currentLogPosterior(0) << ". " << this->currentLogPosterior(1) << "           " << "\r";
                if (this->onDebug) {
                    Rcpp::Rcout << "\n Current values: " << this->currentSample.row(0) << std::endl;
                }
            }
        }
    };

    void init_samplePriorDistributions_discrete(ptmc_discrete::PTMC_D* model, Rcpp::Function samplePriorDistributions) {
        auto func = [samplePriorDistributions](RObject dataList) {
            PutRNGstate();
            auto rData = samplePriorDistributions(dataList);
            GetRNGstate();
            return Rcpp::as<VectorXd>(rData);
        };
        model->samplePriorDistributions = func;
    }

    void init_evaluateLogPrior_discrete(ptmc_discrete::PTMC_D* model, Rcpp::Function evaluateLogPrior) {
        auto func = [evaluateLogPrior](VectorXd params, VectorXi discrete, RObject dataList) {
            PutRNGstate();
            auto rData = evaluateLogPrior(params, discrete, dataList);
            GetRNGstate();
            return Rcpp::as<double>(rData);
        };
        model->evaluateLogPrior = func;
    }

    void init_evaluateLogLikelihood_discrete(ptmc_discrete::PTMC_D* model, Rcpp::Function evaluateLogLikelihood) {
        auto func = [evaluateLogLikelihood](VectorXd params, VectorXi discrete, MatrixXd covariance, RObject dataList) {
            PutRNGstate();
            auto rData = evaluateLogLikelihood(params, discrete, covariance, dataList);
            GetRNGstate();
            return Rcpp::as<double>(rData);
        };
        model->evaluateLogLikelihood = func;
    }

    void init_initialiseDiscrete_discrete(ptmc_discrete::PTMC_D* model, Rcpp::Function initialiseDiscrete) {
        auto func = [initialiseDiscrete](RObject dataList) {
            PutRNGstate();
            auto rData = initialiseDiscrete(dataList);
            GetRNGstate();
            return Rcpp::as<VectorXi>(rData);
        };
        model->initialiseDiscrete = func;
    }

    void init_discreteSampling_discrete(ptmc_discrete::PTMC_D* model, Rcpp::Function discreteSampling) {
        auto func = [discreteSampling](VectorXi discrete, RObject dataList) {
            PutRNGstate();
            auto rData = discreteSampling(discrete, dataList);
            GetRNGstate();
            return Rcpp::as<VectorXi>(rData);
        };
        model->discreteSampling = func;
    }

};
// namespace ptmc
#endif
