
# T (Student) distribution ------------------------------------------------


## compute PDF -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------

# T proba with 2 degrees of freedom.
z <- rjd3toolkit::density_t(10, .01 * seq(-100, 100, 1))
plot(.01 * seq(-100, 100, 1),z,yaxt="n",xaxt="n")  #pb options titres axes à enlever

### R base ----------------------------------------------------------------

# basic R
x <- seq(-5, 5, length.out = 100) # Define a sequence of x values
df <- 2 # Set the degrees of freedom
pdf <- dt(x, df) # Compute the PDF
plot(x, pdf, type="l",
     main="PDF of t-distribution with df degrees of freedom")
# check with rjd
z <- density_t(2, x)
plot(.01 * seq(-100, 100, 1),z,yaxt="n",xaxt="n")

dt(-4:4,2)
density_t(2,-4:4)
#ok



## generate numbers -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------

# T with 2 degrees of freedom. 100 random
# here in rjd no seed
set.seed(123)
z <- random_t(6, 100)
z
plot(z)

### R base ----------------------------------------------------------------
set.seed(123)
n <- 100 # Set the number of random numbers to generate
df <- 6 # Set the degrees of freedom
random_numbers <- rt(n, df) # Generate random numbers
plot(random_numbers)
hist(random_numbers,
     main="Random numbers from t-distribution with 10 degrees of freedom")

# même avec avec le meme seed ne va pas donner les mêmes resultats

## CDF: compute proba -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------

z<-cdf_t(df=12,x=1.2)
z

z<-cdf_t(df=12,x=c(0:10))
z

### R base ----------------------------------------------------------------

x <- seq(-5, 5, length.out = 100) # Define a sequence of x values
x<-c(0:10)
df <- 12 # Set the degrees of freedom
cdf <- pt(x, df) # Compute the CDF
plot(x, cdf,
     type="l", main="CDF of t-distribution with 10 degrees of freedom")
# bizarre
z<-pt(x=1.2,df=12)
pt(1.2,12)
z

# ok verifié


## inv CDF: compute quantiles -------------------------------------------------------------

### rjd3 rien----------------------------------------------------------------

# rien ?

### R base ----------------------------------------------------------------

p <- c(0.025, 0.975) # Define the probabilities for which we want to find quantiles
df <- 10 # Set the degrees of freedom
quantiles <- qt(p, df) # Compute the quantiles
print(quantiles)


# Chi-2 distribution ------------------------------------------------

## compute PDF -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------

# Probability density function of Chi2 with 5 degrees of freedom.
z <- density_chi2(df=5, .01 * seq(-100, 100, 1))
plot(.01 * seq(-100, 100, 1),z)

### R base à refaire  ----------------------------------------------------------------

# basic R
x <- seq(-5, 5, length.out = 100) # Define a sequence of x values
df <- 10 # Set the degrees of freedom
pdf <- dt(x, df) # Compute the PDF
plot(x, pdf, type="l",
     main="PDF of t-distribution with 10 degrees of freedom")
# check with rjd
z <- density_t(10, 0.5)
plot(z)



## generate numbers -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------
set.seed(767)
# T with 2 degrees of freedom. 100 random
# here in rjd no seed
seed(123)
z <- random_chi2(6, 100)
z
plot(z)

### R base ----------------------------------------------------------------
# computing values of 50k random values with 5 degrees of freedom
seed(123)
x <- rchisq(100, df = 6)
plot(x)

hist(x,
     freq = FALSE,
     xlim = c(0,16),
     ylim = c(0,0.2))

# look it up
curve(dchisq(x, df = 5), from = 0, to = 15,
      n = 5000, col= 'red', lwd=2, add = T)

## CDF: compute proba -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------

z<-cdf_chi2(df=12,x=1.2)
z

z<-cdf_chi2(df=12,x=c(0:10))
z

### R base ----------------------------------------------------------------

x <- seq(-5, 5, length.out = 100) # Define a sequence of x values
df <- 10 # Set the degrees of freedom
cdf <- pt(x, df) # Compute the CDF
plot(x, cdf,
     type="l", main="CDF of t-distribution with 10 degrees of freedom")
# bizarre
z<-pt(x=1.2,df=12)
pt(1.2,12)
z

## inv CDF: compute quantiles -------------------------------------------------------------

### rjd3 rien  ----------------------------------------------------------------

# rien ?

### R base ----------------------------------------------------------------

# Gamma distribution ------------------------------------------------


## compute PDF -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------

# Probability density function of Gamma with (\eqn{\alpha=1}) and (\eqn{\theta=2})
z <- density_gamma(1, 2, seq(0,10, 0.1))
plot(seq(0,10, 0.1),z)

### R base ----------------------------------------------------------------

# basic R
x <- seq(-5, 5, length.out = 100) # Define a sequence of x values
df <- 10 # Set the degrees of freedom
pdf <- dt(x, df) # Compute the PDF
plot(x, pdf, type="l",
     main="PDF of t-distribution with 10 degrees of freedom")
# check with rjd
z <- density_t(10, x)
plot(z)



## generate numbers -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------
set.seed(767)
# Generating a random vector with each component drawn from Gamma (\eqn{\alpha=1}, \eqn{\theta=2}) distribution
z <- random_gamma(1,2, 100)
z
plot(z)

### R base ----------------------------------------------------------------
# computing values of 50k random values with 5 degrees of freedom
seed(123)
x <- rchisq(100, df = 6)
plot(x)
v<-x-z
v
x <- rchisq(50000, df = 5)

hist(x,
     freq = FALSE,
     xlim = c(0,16),
     ylim = c(0,0.2))

# look it up
curve(dchisq(x, df = 5), from = 0, to = 15,
      n = 5000, col= 'red', lwd=2, add = T)

## CDF: compute proba -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------

# Computing the probabilty that the random variable X folllowing a Gamma (\eqn{\alpha=1}, \eqn{\theta=2}) is lower than x
z<-cdf_gamma(1,2,x=1.2)
z
z<-cdf_gamma(1,2,x=c(0:10)) # array of values
z

### R base ----------------------------------------------------------------

x <- seq(-5, 5, length.out = 100) # Define a sequence of x values
df <- 10 # Set the degrees of freedom
cdf <- pt(x, df) # Compute the CDF
plot(x, cdf,
     type="l", main="CDF of t-distribution with 10 degrees of freedom")
# bizarre
z<-pt(x=1.2,df=12)
pt(1.2,12)
z


## inv CDF: compute quantiles -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------

# rien ?

### R base ----------------------------------------------------------------

# Inverse Gamma distribution ------------------------------------------------


## compute PDF -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------

# Probability density function of Inverse Gamma (alpha=1 and theta=2)
z <- density_inverse_gamma(1, 2, seq(0,10, 0.1))
plot(z)
class(z)

### R base ----------------------------------------------------------------

# basic R
x <- seq(-5, 5, length.out = 100) # Define a sequence of x values




## generate numbers -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------
set.seed(767)
#' # Generating a random vector with each component drawn from an inverse Gamma (alpha=1 and theta=2) distribution
z <- random_inverse_gamma(1,2, 100)
z
class(z)
plot(z)

### R base ----------------------------------------------------------------
# computing values of 50k random values with 5 degrees of freedom
seed(123)
x <- rchisq(100, df = 6)
plot(x)
v<-x-z
v
x <- rchisq(50000, df = 5)

hist(x,
     freq = FALSE,
     xlim = c(0,16),
     ylim = c(0,0.2))

# look it up
curve(dchisq(x, df = 5), from = 0, to = 15,
      n = 5000, col= 'red', lwd=2, add = T)

## CDF: compute proba -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------
# Computing the probabilty that the random variable X following an Inverse Gamma (alpha=1 and theta=2) is lower than x
z<-cdf_inverse_gamma(1,2,x=1.2)
z
y<-cdf_gamma(1,2,x=1.2)
y
z<-cdf_inverse_gamma(1,2,x=c(1:10)) # array of values
z

### R base ----------------------------------------------------------------

## inv CDF: compute quantiles -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------

# rien ?

### R base ----------------------------------------------------------------



# Inverse Gaussian distribution ------------------------------------------------


## compute PDF -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------

# Probability density function of Inverse Gamma (alpha=1 and theta=2)
z <- density_inverse_gaussian(1, 2, seq(-10,10, 0.1))
plot(seq(-10,10, 0.1),z)
class(z)

### R base ----------------------------------------------------------------

# basic R
x <- seq(-5, 5, length.out = 100) # Define a sequence of x values




## generate numbers -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------
set.seed(767)
# Generating a random vector with each component drawn from an Inverse Gaussian distribution (alpha=1 and theta=2)
z <- random_inverse_gaussian(1,2, 100)
z
class(z)
plot(z)

### R base ----------------------------------------------------------------
# computing values of 50k random values with 5 degrees of freedom
seed(123)


## CDF: compute proba -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------
# Computing the probabilty that the random variable X following an Inverse Gaussian (alpha=1 and theta=2) is lower than x
z<-cdf_inverse_gaussian(1,2,x=1.2)

z<-cdf_inverse_gaussian(1,2,x=0.05)
z
#' z<-cdf_inverse_gaussian(1,2,x=c(1:10)) # array of values
#' z

### R base ----------------------------------------------------------------




## inv CDF: compute quantiles -------------------------------------------------------------

### rjd3 ----------------------------------------------------------------

# rien ?

### R base ----------------------------------------------------------------




