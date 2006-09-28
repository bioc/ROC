/* Calculate sensitivity and specificity of a statistic compared to a
   TRUTH vector.

   Author: Henning Redestig <redestig[at]mpimp-golm.mpg.de
 */

extern "C" {
  // truth -- the truth to compare with
  // data -- the statistic to evaluate
  // cutpts -- the cutpoints to use
  // nt -- the length of truth
  // nc -- the length of the cutpoints
  // spec -- specificity
  // sens -- sensitivity
  void ROC (int *truth, double *data, double *cutpts, 
	    int *nt, int *nc, double *spec, double *sens) {

    int pred [*nt];
    double sensMean;
    double specMean;
    int positives = 0;

    // total amount of positive outcomes
    for(int i = 0; i < *nt; i++) {
      if(truth[i] > 0) {
	positives++;
      }
    }

    // for each cutpoint
    for(int i = 0; i < *nc; i++) {
      // calculate how correct we are
      for(int j = 0; j < *nt; j++) {
	if(data[j] > cutpts[i]) {
	  pred[j] = 1;
	}
	else{
	  pred[j] = 0;
	}
      }

      // now calculate sensitivity/specificity
      sensMean = 0;
      specMean = 0;
      for(int j = 0; j < *nt; j++) {
	if(truth[j] == 1) {
	  sensMean += pred[j];
	}
	else{
	  specMean += 1 - pred[j];
	}
      }
      sens[i] = sensMean / positives;
      spec[i] = specMean / (*nt - positives);
    }
  }

} // extern "C"
