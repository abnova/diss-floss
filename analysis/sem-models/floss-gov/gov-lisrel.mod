Variables: GOV, osi.comp, restrict, vol.contrib

Exogenous latent variables    : GOV
Endogenous latent variables   : Exogenous manifest variables  : osi.comp, restrict, vol.contrib
Endogenous manifest variables : 
m       = (  0.00         0.00        0.00        0.00 )

Phi     = (  VAR_GOV=1.00  ) 


Psi     = ( )

          /  VAR_g1=1.00  0.00        0.00         \
delta   = |  0.00         VAR_g2=1.00 0.00         |
          \  0.00         0.00        VAR_g3=1.00  /


epsilon = ( )

gamma   = ( )

beta    = ( )

          /  1.00          \
lambdaX = |  1.00          |
          \  1.00          /


lambdaY = ( )

Phi     : Variances and covariances of exogenous latent variables.
Psi     : Variances and covariances of endogenous latent variables (containing zeta on the diagonal). 
delta   : Variances and covariances of exogenous manifest variables (also called Theta_delta). 
epsilon : Variances and covariances of endogenous manifest variables (also called Theta_epsilon). 
gamma   : Paths from exogenous to endogenous latent Variables. 
beta    : Paths from endogenous to endogenous latent variables. 
lambdaX : Paths from exogenous latent variables to exogenous manifest variables. 
lambdaY : Paths from endogenous latent variables to endogenous manifest variables. 

All other paths and covariances are zero. 
