Variables: GOV, osi.comp, restrict, vol.contrib

m = (  0.00         0.00        0.00        0.00        )

    /  0.00         0.00        0.00        0.00         \
A = |  1.00         0.00        0.00        0.00         |
    |  1.00         0.00        0.00        0.00         |
    \  1.00         0.00        0.00        0.00         /


    /  VAR_GOV=1.00 0.00        0.00        0.00         \
S = |  0.00         VAR_g1=1.00 0.00        0.00         |
    |  0.00         0.00        VAR_g2=1.00 0.00         |
    \  0.00         0.00        0.00        VAR_g3=1.00  /


    /  0            1           0           0            \
F = |  0            0           1           0            |
    \  0            0           0           1            /


Model Covariance matrix = F (I-A)^{-1} S (I-A)^{-T} F^T
Model Mean Vector       = F (I-A)^{-1} m
