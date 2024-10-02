install.packages('rsconnect')

rsconnect::setAccountInfo(name='danielcamara', token='D0DE5C7B8104A8A20240E7C981559534', secret='C580Xl0MimrspsgMI21fUSDs+v8VQ/gfXzF0MDjU')

library(rsconnect)
rsconnect::deployApp('../Dashboard InfoGripe - Online/')
