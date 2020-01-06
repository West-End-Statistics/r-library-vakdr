#include <Rcpp.h>
using namespace Rcpp;

//' windowThreshold Function
//'
//' @param meas_col the numeric column of the measure
//' @param meas_norm a numeric vector showing the c(low, high) acceptable range
//' @param time_col the time column, as a numeric
//' @param look_backward the amount to look backward for an invalid value
//' @param look_forward the amount to look forward for an invalid value
//'
//' @return logical vector for each row in the initial time row
//' @export
//'
//' @examples
//' d <- data.frame(
//' measure = 1:10,
//' time = 0:9
//' )
//' library(dplyr)
//' d %>% mutate(hit = windowThreshold(measure, c(2,8), time, 2, 0))
//'
//'
// [[Rcpp::export]]
LogicalVector windowThreshold(NumericVector meas_col, NumericVector meas_norm, NumericVector time_col,
                               double look_backward, double look_forward) {

  int n = time_col.size();
  LogicalVector out(n);

  bool foundSomething = false;
  // bool outsideTime = false;


  for(int i = 0; i < n; ++i){
    //loop over every timepoint

    // check if current value hits the alert
    foundSomething = (meas_col[i] < meas_norm[0]) | (meas_col[i] > meas_norm[1]);
    if(foundSomething){
      // no need to check before/after if current is an alert
      out[i] = true;
      continue;
    }

    // check look backward
    if(look_backward > 0){
      double l_t = time_col[i] - look_backward;

      for(int j = i - 1; j >=0; --j){
        if(time_col[j] < l_t){
          // if we are too far backwards, stop checking and end loop
          break;
        }
        foundSomething = (meas_col[j] < meas_norm[0]) | (meas_col[j] > meas_norm[1]);
        if(foundSomething){
          // no need to check earlier values
          out[i] = true;
          break;
        }

      }
    }

    // check look forward
    if((look_forward > 0) & !foundSomething){
      double u_t = time_col[i] + look_forward;

      for(int k = i + 1; k < n; ++k){
        if(time_col[k] > u_t){
          // if we are too far forwards, stop checking and end loop
          break;
        }
        foundSomething = (meas_col[k] < meas_norm[0]) | (meas_col[k] > meas_norm[1]);
        if(foundSomething){
          // no need to check earlier values
          out[i] = true;
          break;
        }

      }
    }

  }
  return(out);
}
