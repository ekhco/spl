* comparing competing risk regression to R
stset t_event, failure(event == 1)
stcrreg agedx_IPL, compete(event == 2) nolog

//      failure event:  event == 1
// obs. time interval:  (0, t_event]
//  exit on or before:  failure

// ------------------------------------------------------------------------------
//      10,000  total observations
//         380  observations end on or before enter()
// ------------------------------------------------------------------------------
//       9,620  observations remaining, representing
//         142  failures in single-record/single-failure data
//  15,897.465  total analysis time at risk and under observation
//                                                 at risk from t =         0
//                                      earliest observed entry t =         0
//                                           last observed exit t =  16.02782

// . 
// . stcrreg agedx_IPL, compete(event == 2) nolog

//          failure _d:  event == 1
//    analysis time _t:  t_event

// Competing-risks regression                       No. of obs       =      9,620
//                                                  No. of subjects  =      9,620
// Failure event  : event == 1                      No. failed       =        142
// Competing event: event == 2                      No. competing    =      8,633
//                                                  No. censored     =        845

//                                                  Wald chi2(1)     =      53.89
// Log pseudolikelihood = -1252.1528                Prob > chi2      =     0.0000

// ------------------------------------------------------------------------------
//              |               Robust
//           _t |        SHR   Std. Err.      z    P>|z|     [95% Conf. Interval]
// -------------+----------------------------------------------------------------
//    agedx_IPL |    .912767   .0113491    -7.34   0.000      .890792    .9352841
// ------------------------------------------------------------------------------
