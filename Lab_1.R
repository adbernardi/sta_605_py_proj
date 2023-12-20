####################################
# 605 - Lab 1 - On Your Own Version
# Anthony Bernardi
# August 25th, 2023
####################################

####################################
# Question 1 - Working with Objects in R
####################################

# specify four variables 
x <- 4
y <- 6
z <- 0.57
a <- 3.7

# combining these into a 2x2 matrix with x,y in the first row 
# z, a in the second row 
mat <- matrix(data = c(x , y , z , a), nrow=2, ncol=2 , byrow = TRUE)

mat

# specify another vector (length 100)
# values of mat replicated 25 times 

# just going to rep each element individually 
xrep <- rep(x, 25)
xrep
yrep <- rep(y, 25)
zrep <- rep(z, 25)
arep <- rep(a, 25)

arep

mat_rep <- matrix(data = c(xrep , yrep , zrep , arep), nrow=2, ncol = 50, byrow = TRUE)

mat_rep
length(mat_rep) # verifying the length is 100

#################################### 
# Question 2 - Estimating probabilities using proportions
####################################

# Generate a length 10 vector of numbers randomly sampled from 
# 1 to 10 with replacement 

?sample()
target <- c(1:10)

sample(target, 10, replace = TRUE) # now want to store this 

our_sample <- sample(target, 10, replace = TRUE)
our_sample
length(our_sample)

# testing things out with some pseudocode 
first_element <- our_sample[1] > 4
first_element
second_element <- our_sample[2] > 4
second_element
test_vec <- c(first_element , second_element)

test_vec
our_sample # logical check, seems to work as the idea for the body of the loop

# For loop to check if each value is greater than 4, 
# then storing that in a in another vector, also length 10 

for (i in our_sample){
  t_f_vec <- our_sample > 4
  print(t_f_vec)
}

t_f_vec

our_sample
our_sample > 4
length(our_sample > 4)

# this works, but not the best design, so will have to revisit 

# What proportion of numbers drawn in part A are greater than 4?

prop_true <- sum(our_sample > 4) / length(our_sample)
prop_true # 0.6 True Values

# Repeat parts A-C w/ a vector of length 10,000

# generate a length now 10,000 vector with numbers randomly sampled from integers 1-10, with replacement 

1e4
target
our_sample_big <- sample(target, 1e4, replace = TRUE)
our_sample_big
length(our_sample_big) # just verifying 

# doing the for loop step again
for (i in our_sample_big){
  
  t_f_vec_big <- our_sample_big > 4
  print(t_f_vec_big)  
}

our_sample_big

# doing the proportion again 
prop_true_big <- sum(our_sample_big > 4) / length(our_sample_big)
prop_true_big # 0.6004% True values 

boolean_prop_test <- sum(t_f_vec_big) / length(t_f_vec_big)
boolean_prop_test # same value, so seems to work even if it's not the most efficient 
# verify this same proportion calculation with the t/f vector in the big case 


# E - What proportion is closer to 0.6, the true probability? Is this what you'd expect?

# In this case, the vector of length 10 was exactly 0.6, which is not what I would expect, to be honest. The larger vector having a value of 0.6004, as I would expect 
# the sample value to converge to 0.6 with additional sampling.

#################################### 
# Question 3 - Ride to the Stats Dept Picnic
####################################

# vehicle for carpool has 8 total seats 
# prob that if your friend drives 7 of her friends, what is the prob 
# you get a window seat
# everyone sits randomly, use simulation to estimate prob 

# represent the seats in a car as a 3x3 matrix 
# just do row vectors for the cars

front_row <- c("d", "x", "w")
middle_row <- c("w", "n", "w")
back_row <- c("w", "n", "w")

suv_seating <- c(front_row , middle_row , back_row)

?matrix()
suv_mat <- matrix(suv_seating , nrow = 3 , ncol = 3, byrow = TRUE)
suv_mat

?sample()

# so we have suv_mat for a representation, but are going to change the proportion for sampling
# considering you can't drive, and there's no front middle seat 

seating_array <- c("w", "w", "n", "w", "w", "n", "w")

seating_array

# making sure the proportions are right 
(sum(seating_array == "w") / length(seating_array)) # ~71% true chance of getting window seat

# A  - Of a 10-time sample, how many times do you get the middle seat?

small_sample <- sample(seating_array , 10 , replace = TRUE)
small_sample

# now finding how often you sit by a window seat 

sum(small_sample == "w")

# tokenizing in this way to get a proportion

window_times_small <- sum(small_sample == "w") / length(small_sample)
window_times_small # in this case, 60% of the time you're sitting by a window

# B - Randomly place yourself in the car 500 times, how often do you sit by a window?
# doing the same thing but 500 times 
big_sample <- sample(seating_array , 500 , replace = TRUE)
big_sample
table(big_sample) # want to be sure we're only getting window/non-window

# getting a proportion with the larger sample now 

window_times_big <- sum(big_sample == "w") / length(big_sample)
window_times_big # Here, getting a window ~67% of the time 

suv_mat # our True value is 5/7, or ~71 pct

# window_times_big proportion is 0.67, so this converges with our true value 

# C: Challenge - friend is late based on a normal distribution 
# x minutes late with mean 5 and variance 2

?rnorm()

rnorm(1, mean = 5 , sd = sqrt(2))

# if the student is more than 6 mins late, one of the middle seats is empty 
# if not, person gets in the car and sits in a random seat 

# maybe a for loop for eventually doing this 10 times, but for now we'll do pseudocode 

friend_time <- rnorm(1, mean = 5, sd = sqrt(2))
friend_time

# if friend_time > 6

seating_array
# need an array for if/when the friend is late 
seating_late_friend <- c("w", "w", "w", "n", "w")
seating_late_friend

# for loop just to do something 10 times?
for (i in 1:10){
  print(i)
}


# trying to build own function for this 
carpool_seating <- function(n){
  for (i in 1:n){
    friend_time <- rnorm(1, mean = 5, sd = sqrt(2))
    where_sit <- list()
      if (friend_time < 6){
        on_time <- sample(seating_array, 1, replace = TRUE)
        where_sit[i] <- on_time
    # print(sum(on_time == "w") / length(on_time))
      if (friend_time > 6){
        late <- sample(seating_late_friend , 1 , replace = TRUE)
        where_sit[i] <- late
     # print(sum(late == "w") / length(late))
    print(where_sit)
    }
  }
 }
}

# just trying to do this with a for loop 

n_samples <- 10
friend_time_list <- vector(mode = "list", length = n_samples)
friend_time_list[5]
for(i in 1:10){
  friend_time[i] <- rnorm(1, mean = 5 , sd = sqrt(2))
  friend_time_list[i] <- friend_time[i]
  print(friend_time_list)
}
str(friend_time_list)

which_seat <- vector(mode = "list" , length = n_samples)
which_seat
for (i in 1:10){
  on_time <- 0
  late <- 0
    if (friend_time[i] < 6){
      on_time[i] <- sample(seating_array , 1 , replace = TRUE)
      which_seat[i] <- on_time[i]
    
    } else{
      late[i] <- sample(seating_late_friend , 1 , replace = TRUE)
      which_seat[i] <- late[i]
  }
  print(which_seat)
}

which_seat
length(which_seat)
sum(which_seat == "w") / length(which_seat)
sum(which_seat == "w")

# In a 10 time scenario, I get the window seat ~70% of the time, which makes sense 
# my odds should go up with a late friend 
which_seat


# now trying this 1000 times 
# In 1000 simulations, how many times do you get the window seat with a late friend?
n_samples_big <- 1e3

?list()
?rep()

# initializing empty lists for later storage with a for loop 
friend_time_list_big <- rep(0 , length.out = n_samples_big) 
friend_time_list_big
length(friend_time_list_big)
table(friend_time_list_big)
friend_time_big <- rep(0 , length.out = n_samples_big)
class(friend_time_big)
str(friend_time_big)
hist(friend_time_big)

# something wrong with the loop...
# going to try two separate loops to get a list of 
# how late the friend is, and then will do a separate loop for the 
# actual seating 
for(i in 1:1e3){
  friend_time_big[i] <- rnorm(1, mean = 5 , sd = sqrt(2))
  friend_time_list_big[i] <- friend_time_big[i]
  print(friend_time_list_big)
}
str(friend_time_list_big)

hist(friend_time_list_big)

# initializing empty lists to store the seats you end up getting in these 
# simulations 
which_seat_big <- rep(NA, length.out = n_samples_big)
as.list(which_seat_big)
class(which_seat_big)
str(which_seat_big)
friend_time_big[70] # checking the list is right 

# now making a loop for the actual recording and storing of which 
# seat you'll get 
for (i in 1:1e3){
  on_time_big <- 0
  late_big <- 0
  if (friend_time_big[i] < 6){
    on_time_big[i] <- sample(seating_array , 1 , replace = TRUE)
    which_seat_big[i] <- on_time_big[i]
    
  } else{
    late_big[i] <- sample(seating_late_friend , 1 , replace = TRUE)
    which_seat_big[i] <- late_big[i]
  }
  print(which_seat_big)
}

table(which_seat_big) # eyeballing the proportions after our loop
sum(which_seat_big == "w")

# now doing a proportion 
sum(which_seat_big == "w") / length(which_seat_big) # ~74% , seems right 


#### end of code for submission - Lab 1 - On Your Own Version


####################################
# Scratch buffer
####################################

# Commenting out everything below this. I tried to build my own function 
# but found a for loop more efficient 
# carpool_seating(n = 10)

#for (i in 1:10){
  #storage <- c()
  #storage[i] <- rnorm(1, mean = 5 , sd = sqrt(2))
#  print(storage)
# sample(seating_array , 1 , replace = TRUE)
