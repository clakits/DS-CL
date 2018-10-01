install.packages("BHH2")
library(BHH2)
# facotorial design with k numbers
print(ffDesMatrix(2))

print(ffDesMatrix(3))
print(ffDesMatrix(4))
print(ffDesMatrix(5))
# example
data(shoes.data)
summary(shoes.data)
shoes