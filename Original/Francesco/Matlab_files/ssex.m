% =============================================================================
%
%                   STEADY STATE CONTROLLABILITY ANALYSIS
%                   -------------------------------------
%                  
%  User input required:
%
%
%    g an overall steady-state gain matrix linking potential manipulated
%    variables and disturbances, u, to measured variables, y, (unaltered
%    by program). This matrix should be generated with all variables at
%    their normal steady state operating point.
%
%    isp : a row of indices for measured variables for which setpoint
%    changes are required. Indices correspond to rows of g.
%
%    yss, uss : steady state values of outputs and inputs
%
%
% **************************************************************************
% *  Based on this information a default controllability analysis          *
% *  problem is defined as below by running the command ssdef             * 
% *                                                                        *
% *  yh(i) (upper bound on y(i)) 1.e6*(yss(i)) if yss(i) is positive       *
% *                             -1.e6 yss(i) if yss(i) is negative         *
% *  yl(i) (lower bound on y(i)) 0. if yss(i) is positive;                 *
% *                              1.e6*yss(i) if yss(i) is negative         *
% *                                                                        *
% *  All default bounds have a minimum absolute value of one.              *
% *  I.e. positive variables are constrained between zero and three times  *
% *  the steady state value and negative variables are constrained         *
% *  between minus the steady state value and twice the steady state value *
% * 									   *
% *  rangey(i) : desired range of variability in y(i) used for evaluating  *
% *  strength of coupling between inputs and outputs. N.B. this is not     *
% *  the difference between the upper and lower limits on y(i) used for    *
% *  disturbance rejection analysis, but is an estimate of the range over  *
% *  which the variable y(i) needs to adjustable with the inputs used for  *
% *  control. This estimate allows "weak" connections to be identified.    *
% *  The default is  frac_range times the steady-state value, with a    *
% *  minimum of 1.e-10. frac_range should normally be .2                   *
% *                                                                        *  
% *  uh (upper bound on u)       As for                                    *
% *  ul (lower bound on u)         y                                       *
% *                                                                        *
% *  spl (lower bound on setpoint change) default -0.1*yss([isp])          *
% *  spu (upper bound on setpoint change) default  0.1*yss([isp])          *
% *									   * 
% *  bias defines the expected measurement bias (steady state noise)       *
% *  and defaults to frac_noise*yss                                        *
% *  default +/- .1 percent of steady-state, with minimum of 1.e-10        *
% *                                                                        *
% **************************************************************************
% 
%   The user may then overwrite any of the defaults, to customise the 
%   problem definition:
%
%   the disturbances, manipulated and measured variables are then selected
% 
%   ic a row of indices for measured values to be controlled with zero 
%   offset. Indices correspond to rows of g.
%
%   iu a row of indices for manipulated variables to be used for control 
%   Indices correspond to columns of g.
%
%   id a row of indices for disturbances to be considered.
%   Indices correspond to columns of g.
%
%
% *************************************************************************** 
% * Before the analysis is executed the program ssconsist is executed      *
% * This further checks the user input for consistency                      *
% * E.g. If the bounds on a controlled element of y are tighter than the    *
% * bias value for that measurement,a warning is issued and the program will*
% * stop                                                                    *
% ***************************************************************************
%
%
%  The analysis program ssanal is then executed and provides the following
%  information:
%
%  Warnings of potential structural flaws in the control structure selected
%  The number of effective degrees of freedom (directions in which the selected
%  outputs can be manipulated over their full range) is calculated
%
%     **** the additional details below are given only if iprint=2 ****
%     **** the scaled single input and output effectiveness provide****
%     **** similar information more straightforwardly *****************
%
%     Manipulated variables which have a weak affect on all the outputs
%     (may be able to simplify the control scheme by dropping this input or
%     improve it by exchanging this input for another)
%
%     Controlled variables which are weakly affected by all the manipulated
%     variables.
%     (If the output corresponds to a key objective then it may be necessary
%     to find another manipulated variable to add to iu or increase the 
%     actuator capacity of an existing manipulated variable --- the variable
%     having the strongest effect will be noted.
%     If the output does not correspond to a key objective then it should
%     be dropped if the strongest effect is very small or a reduced 
%     measurement range used if the effect is moderate) 
%
%     Groups of manipulated variables which are almost linearly dependent 
%     (act in same direction) listed from the strongest to the weakest
%     (may wish to simply drop the weakest dependent input as it is likely to
%     complicate rather than improve the control, alternatively may wish
%     to combine this weak dependent input with another dependent input
%     in a split-ranging mode or otherwise) 
%     
%     Groups of outputs which are almost linearly dependent 
%     (move in same direction in response to the inputs)
%     (It is unlikely that all the dependent measurements can be controlled
%     independently. It is also likely that if one of the dependent
%     measurements is dropped from the controlled set ic it will remain
%     tightly controlled if the others are controlled. This is checked by
%     examining whether the outputs show a similar dependence with respect to
%     the disturbances, and a message is issued.
%
%     This information provides some assistance in modifying the control 
%     structure. 
%
%  ******************  Always calculated *******************************
%  The SIE, SSIE and SSOE are calculated as appropriate. Values near zero
%  for SIE/SSIE indicate manipulated variables which can be readily 
%  substituted for by other variables in the control structure analysed.
%  Values near 1 indicate variables which have an effective contribution to 
%  make which cannot be replaced by other variables among those selected
%
%  *******if iprint is one or two *************************************
%  The minimised condition number of the control structure selected.
%  Values above 50 are suggestive of substantial interactions and should
%  be avoided where possible. A control specialist should be consulted if
%  this cannot be achieved.
%
% **************always calculated **************************************** 
%  A test to check if all constraints are expected to be satisfied is
%  executed. This test examines the range of variation on both inputs and 
%  outputs assuming perfect control of the controlled variables y(ic) and
%  minimising the sum of squares for manipulated variable adjustments 
%  normalised by their range.
%  Variables whose range exceeds their limits are always displayed showing
%  predicted range and declared limits
%  if iprint=1 or 2 all variables other than disturbances are displayed in
%  this way.
%
% **************************************************************************
% * All the above results relate to the overall choice of manipulated and  *
% * measured variables. They bear no relationship to pairing decisions.    *
% **************************************************************************
% 
%  The analysis then evaluates the transfer function between the 
%  "non-stabilising" variables in the control structure with the first nus
%  "core" control variables assumed to be perfectly controlled.
%  N.B. It is essential to the interpretation of the RGA and Niederlinski
%  index that the system analysed contains no integrators and is stable.
%  (This is usually achievable by controlling all non-self-regulating levels
%  and key variables such as temperature in exothermic reactors, and by
%  including the measurements and manipulated variables used for this 
%  purpose in the core set)
%
%  An RGA with diagonal elements between .1 and 10 is highly desirable.
%  If this cannot be achieved a control specialist should be called in.
%
%  Negative elements on the diagonal of the RGA, or a negative Niederlinski
%  index, with ic and iu ordered so that the ith element of ic is paired
%  with the ith element of iu, indicate problems with system integrity and 
%  potential commissioning difficulties.
%
%  Finally, the steady-state gain matrix for the non-stabilising structure
%  with the stabilising structure in place is calculated
%           (a) scaled as for strength of effect analysis (gsubs) 
%           (b) unscaled (gsubu)
%  and the overall gain matrix
%           (a)  scaled as for singular value analysis (gs)
%           (b)  unscaled (gu)
%  The scaling used is that the inputs are normalised by their range 
%  (defined by upper and lower limits) and the ouputs are normalised by rangey
%  (default 20% (frac_range) of steady state value).
%
%  N.B. these matrices are not automatically displayed but can be examined by 
%  typing their names in MATLAB after running the program
%
%  ***************for iprint=2***********************************************
%  An analysis of the gains is carried out comparing the gains for each set
%  of paired values with 
%       (a) the total gain from all  manipulated variables to the controlled 
%       variable in the loop considered and
%       (b) the total disturbance effect
%  All inputs are normalised by their range
%  The analysis is carried out based on the open-loop gains and then based
%  on the gains with the core loops closed
% 
%----------------------------------------------------------------------------
%----------------------------------------------------------------------------
%
% The Tennessee Eastman problem is used below as an example
%
% outputs are measurements 1 to 41, inputs 1-12 are manipulated variables,
% inputs 13-19 are disturbances.
% load matrix g of steady-state gains)
load g.dat
%
% add mass flow G/ mass flow H (molecular weight*y(40)/molecular weight*y(41))
% as a new measurement (y(42))
% d(Gm/Hm)/du=1/Hm MWG dG/du- Gm/Hm^2MWH dH/du
%
g(42,:)=g(40,:)*62.0/(43.828*76)-(53.724*62.)/(43.828*76)^2 *76*g(41,:);
%
% set up steady-state values
%
yss=[.25052 3664. 4509.3 9.3477 26.902 42.239 2705. 75.,120.4 .33712 ...
 80.109 50. 2633.7 25.16 50. 3102.2 22.949 65.731 230.31 341.43 94.599 ...
 77.297 32.188 8.8933 26.383 6.8820 18.776 1.6567 32.958 13.823 23.978 ...
 1.2565 18.579 2.2633 4.8436 2.2986 0.01787 0.8357 0.09858 53.724 43.828 ...
  1.];
uss=[ 63.053 53.98 24.644 61.302 22.210 40.064 38.1 46.534 47.446 41.106 18.114 50. zeros(1,11)];
%
% define variables to be controlled to setpoints
isp=[17 42 7 30];
%
% N.B. yss, uss and isp must be defined before ssdefault. Any other information
% placed before this point is liable to be overwritten.
% frac_noise defines the fraction of steady-state used for the default
% noise level.
frac_noise=.002;
%
% frac_noise defines the fraction of steady-state used for the default
% target range for y in evaluating degrees of freedom and scaled
% input and output effectiveness
frac_range=.2;
%
% iprint defines the print level 0 = standard, 1= more detail, 2=maximum detail
ssdef
%
% problem specific limits
%
yh(7)=2895;
yh(8)=100.;
yl(8)=50.;
yh(9)=150.;
yh(12)=100;
yl(12)=30;
yh(15)=100;
yl(15)=30;
%
% put in specific setpoint ranges from problem definition.
sph(1)=0.;
spl(1)=-3.744;
sph(2)=0.;
spl(2)=-.333;
sph(3)=0.;
spl(3)=-60.;
spl(4)=0.;
sph(4)=2.;
% manipulated variables lie between 0 and 100 and disturbances between 0 and 1
% the zeros and ones commands are a quick way of generating matrices of zeros
%
ul=zeros(1,19);
uh=[100*ones(1,12) ones(1,7)];
%
% define control structure
% first nus variables of ic and iu are the "stabilising"/"core" structure
%
nus=4;


ic=[9 8 12 15 17 42 7 30 23];
iu=[10 2 11 7 8 1 4 6 3] ;


%
% IDV(1) and IDV(4)
id=[13  16];
sscon
iprint=0;
ssanal
