Variables: x1, x1', x1'

m = (  0.00        0.00 0.00 )

    /  0.00        0.00 0.00  \
A = |  0.00        0.00 0.00  |
    \  1.00        1.00 0.00  /


    /  VAR_x1=1.00 0.00 0.00  \
S = |  0.00        0.00 0.00  |
    \  0.00        0.00 0.00  /




Model Covariance matrix = F (I-A)^{-1} S (I-A)^{-T} F^T
Model Mean Vector       = F (I-A)^{-1} m
