# Bootrsapped skewness-adjusted t-test

Implementation in R of the Bootstrapped Skewness-Adjusted t-test for testing long run mean abnormal returns as in "[Improved Methods for Tests of Long-Run Abnormal Stock Returns](http://doi.org/10.1111/0022-1082.00101)" by Lyon et al (1999).

## Usage

    install_github('waissbluth/skewttest')
    library('skewttest')
    # Generate 100 samples from a gamma distribution
    x <- rgamma(100,2,1)
    # Perform a skewed t-test
    skewt.test(x, mu=2)
    # Compare to traditional t-test
    t.test(x, mu=2)


## Author
Nicolás Waissbluth

## License
GNU General Public License v3.0
