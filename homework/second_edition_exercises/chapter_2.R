# 2E1 
# Which of the expressions below correspond to the statement: the probability of rain on Monday?
# (1) Pr(rain)
# -> (2) Pr(rain|Monday)
# (3) Pr(Monday|rain)
# -> (4) Pr(rain, Monday)/ Pr(Monday)

# 2E2. Which of the following statements corresponds to the expression: Pr(Monday|rain)?
# (1) The probability of rain on Monday.
# (2) The probability of rain, given that it is Monday.
# -> (3) The probability that it is Monday, given that it is raining.
# (4) The probability that it is Monday and that it is raining.

# 2E3. Which of the expressions below correspond to the statement: the probability that it is Monday,
# given that it is raining?
# -> (1) Pr(Monday|rain)
# (2) Pr(rain|Monday)
# (3) Pr(rain|Monday) Pr(Monday)
# -> (4) Pr(rain|Monday) Pr(Monday)/ Pr(rain)
# (5) Pr(Monday|rain) Pr(rain)/ Pr(Monday)

# 2E4. The Bayesian statistician Bruno de Finetti (1906-1985) began his book on probability theory
# with the declaration: “PROBABILITY DOES NOT EXIST.” The capitals appeared in the original, so
# I imagine de Finetti wanted us to shout this statement. What he meant is that probability is a device
# for describing uncertainty from the perspective of an observer with limited knowledge; it has no
# objective reality. Discuss the globe tossing example from the chapter, in light of this statement. What
# does it mean to say “the probability of water is 0.7”?

# 2M1. Recall the globe tossing model from the chapter. Compute and plot the grid approximate
# posterior distribution for each of the following sets of observations. In each case, assume a uniform
# prior for p.
# (1) W, W, W
# (2) W, W, W, L
# (3) L, W, W, L, W, W, W

# 2M2. Now assume a prior for p that is equal to zero when p < 0.5 and is a positive constant when
# p ≥ 0.5. Again compute and plot the grid approximate posterior distribution for each of the sets of
# observations in the problem just above.

# 2M3. Suppose there are two globes, one for Earth and one for Mars. The Earth globe is 70% covered
# in water. The Mars globe is 100% land. Further suppose that one of these globes—you don't know
# which—was tossed in the air and produced a “land” observation. Assume that each globe was equally
# likely to be tossed. Show that the posterior probability that the globe was the Earth, conditional on
# seeing “land” (Pr(Earth|land)), is 0.23.

# 2M4. Suppose you have a deck with only three cards. Each card has two sides, and each side is either
# black or white. One card has two black sides. The second card has one black and one white side. The
# third card has two white sides. Now suppose all three cards are placed in a bag and shuffled. Someone
# reaches into the bag and pulls out a card and places it flat on a table. A black side is shown facing up,
# but you don't know the color of the side facing down. Show that the probability that the other side is
# also black is 2/3. Use the counting method (Section 2 of the chapter) to approach this problem. This
# means counting up the ways that each card could produce the observed data (a black side facing up
# on the table).

# 2M5. Now suppose there are four cards: B/B, B/W, W/W, and another B/B. Again suppose a card is
# drawn from the bag and a black side appears face up. Again calculate the probability that the other
# side is black.

# 2M6. Imagine that black ink is heavy, and so cards with black sides are heavier than cards with white
# sides. As a result, it's less likely that a card with black sides is pulled from the bag. So again assume
# there are three cards: B/B, B/W, and W/W. After experimenting a number of times, you conclude that
# for every way to pull the B/B card from the bag, there are 2 ways to pull the B/W card and 3 ways to
# pull the W/W card. Again suppose that a card is pulled and a black side appears face up. Show that
# the probability the other side is black is now 0.5. Use the counting method, as before.

# 2M7. Assume again the original card problem, with a single card showing a black side face up. Before
# looking at the other side, we draw another card from the bag and lay it face up on the table. The face
# that is shown on the new card is white. Show that the probability that the first card, the one showing
# a black side, has black on its other side is now 0.75. Use the counting method, if you can. Hint: Treat
# this like the sequence of globe tosses, counting all the ways to see each observation, for each possible
# first card.

# 2H1. Suppose there are two species of panda bear. Both are equally common in the wild and live
# in the same places. They look exactly alike and eat the same food, and there is yet no genetic assay
# capable of telling them apart. They differ however in their family sizes. Species A gives birth to twins
# 10% of the time, otherwise birthing a single infant. Species B births twins 20% of the time, otherwise
# birthing singleton infants. Assume these numbers are known with certainty, from many years of field
# research.
# Now suppose you are managing a captive panda breeding program. You have a new female panda
# of unknown species, and she has just given birth to twins. What is the probability that her next birth
# will also be twins?

# 2H2. Recall all the facts from the problem above. Now compute the probability that the panda we
# have is from species A, assuming we have observed only the first birth and that it was twins.

# 2H3. Continuing on from the previous problem, suppose the same panda mother has a second birth
# and that it is not twins, but a singleton infant. Compute the posterior probability that this panda is
# species A.

# 2H4. A common boast of Bayesian statisticians is that Bayesian inference makes it easy to use all of
# the data, even if the data are of different types.
# So suppose now that a veterinarian comes along who has a new genetic test that she claims can
# identify the species of our mother panda. But the test, like all tests, is imperfect. This is the informa-
# tion you have about the test:
# • The probability it correctly identifies a species A panda is 0.8.
# • The probability it correctly identifies a species B panda is 0.65.
# The vet administers the test to your panda and tells you that the test is positive for species A. First
# ignore your previous information from the births and compute the posterior probability that your
# panda is species A. Then redo your calculation, now using the birth data as well.


grid_approximation <- function(grid_size = 20, positive_data_size, data_size, prior_name = NULL, output_name = "chapter_2") {

	# Create grid points
	p_grid <- seq(from = 0, to = 1, length.out = grid_size)

	# Define prior
	if (is.null(prior_name) || prior_name == 'Uniform'){
		prior = rep(1, grid_size)
		prior_name = 'Uniform'
	} else if (prior_name == 'Constant') {
	   prior = ifelse(p_grid < 0.5, 0, 1)
	}

	# Compute likelihood at each point in grid 
	likelihood <- dbinom(positive_data_size, data_size, prob=p_grid)

	# Compute posterior
	posterior <- prior * likelihood
	posterior = posterior / sum(posterior)

	# Plot posterior distribution
	png(paste('images',paste(output_name, 'png', sep='.'), sep = '/'))

	plot(p_grid, posterior, type="b", xlab="Percentage of water", ylab="Posterior probability")
	mtext(paste(grid_size, " points -", positive_data_size, '/', data_size, ' success -', prior_name))

	dev.off()
}

#2M1
grid_approximation(1000, 3, 3, output_name="chapter2_2M1_1")
grid_approximation(1000, 3, 4, output_name="chapter2_2M1_2")
grid_approximation(1000, 5, 7, output_name="chapter2_2M1_3")

#2M2
grid_approximation(1000, 3, 3, prior_name = 'Constant', output_name="chapter2_2M2_1")
grid_approximation(1000, 3, 4, prior_name = 'Constant', output_name="chapter2_2M2_2")
grid_approximation(1000, 5, 7, prior_name = 'Constant', output_name="chapter2_2M2_3")

#2M3
prob_water_given_earth = 0.7
prob_water_given_mars = 0
prob_land_given_earth = 1 - prob_water_given_earth
prob_land_given_mars = 1 - prob_water_given_mars

prob_earth = 0.5
prob_mars = 1 - prob_earth

prob_land = prob_land_given_earth * prob_earth + prob_land_given_mars * prob_mars

prob_earth_given_land = (prob_land_given_earth * prob_earth) / prob_land

print(paste('2M3. Probability of earth given land:', prob_earth_given_land))

#2M4
# Available cards: B/B, B/W, W/W
# Selected card: B/?
# Show that prob of ? being B is 2/3

print("2M4. Given that the selected card shows B, it can only be the B/B card or the B/W card. If it was the B/B card, then it could be the B1 or B2 side, meaning that I could have 2 ways that the other side is B. If it was the B/W card, then I have no ways of having B on the other side. So there are 2 ways out of 3 to have B on the other side")

#2M5
# Available cards: B/B, B/B, B/W, W/W
# Selected card: B/?
# Show that prob of ? being B is 4/5

print("2M5. Starting from the previous probability, having a new B/B adds 2 ways of having B on the other side, meaning that the probability is now 4/5")

#2M6
# Available cards: B/B, B/W, W/W
print("2M6. Now the B/W card can appear twice the time of the BB card, meaning that there are 2*1 more ways to draw this card and show the B side. Adding them to 2M4 means that now there are 4 ways of having the B side show, 2 from the B/B card and 2 from the B/W card -> 2/4 prob that the other side is B (the B/B card)")

#2M7
print("2M7. Assuming the first card is the B/B, there are 2 (the B in B/B) * 3 (the W in W/W and W/B) = 6 ways to observe BW data. If the first card is the B/W, there are 1 (B in B/W) * 2 (W in W/W and B/B) = 2 ways to observe the BW data. If the first card is the W/W, there are no ways to observe the data. So the total ways to observe the data is 6+2 and only in 6 of them the first card is the B/B")

#2H1
prob_A = 0.5
prob_B = 1- prob_A

prob_twins_given_A = 0.1
prob_singleton_given_A = 1 - prob_twins_given_A
prob_twins_given_B = 0.2
prob_singleton_given_B = 1 - prob_twins_given_B

prob_twins = prob_twins_given_A*prob_A + prob_twins_given_B*prob_B
prob_twins_twins = prob_twins_given_A*prob_twins_given_A*prob_A + prob_twins_given_B*prob_twins_given_B*prob_B  # p(twins2, twins1)
print(paste('2H1. Probability of second twins after first twins:', prob_twins_twins/prob_twins))

#2H2
prob_A_given_twins = prob_twins_given_A * prob_A / prob_twins
print(paste('2H2. Probability of species A given the birth of twins:', prob_A_given_twins))

#2H3
# Note that this step is calculated using the 2H2 posterior as new prior for prob of species A and B
prob_singleton = prob_singleton_given_A * prob_A_given_twins + prob_singleton_given_B * (1 - prob_A_given_twins)

prob_A_given_singleton = prob_singleton_given_A * prob_A_given_twins / prob_singleton 	#NOTE: using prob_A_given_twins and not prob_A since Bayesian updating allow using the previous posterior (2H2) as new prior

print(paste('2H3. Given that the 2H2 posterior can be used as prior for the prob of species A (and species B as 1 - prob A), the updated probability of A after a twin and singleton birth is:', prob_A_given_singleton))

#2H4
prob_testPositiveA_given_A = 0.8
prob_testPositiveB_given_B = 0.65
prob_testPositiveA_given_B = 1 - prob_testPositiveB_given_B
prob_testPositiveB_givenA = 1 - prob_testPositiveA_given_A


prob_testPositiveA = prob_testPositiveA_given_A * prob_A + prob_testPositiveA_given_B * prob_B

# Not considering birth data
prob_A_given_testPositiveA = prob_testPositiveA_given_A * prob_A / prob_testPositiveA #NOTE: not considering birth data, so using initial 0.5 prob of species

print(paste('2H4. 1) Not considering birth data, the probability of species A given a positive test is :', prob_A_given_testPositiveA))

# Considering birth data, meaning that I use the posterior of 2H3 as new prior for A
prob_testPositiveA = prob_testPositiveA_given_A * prob_A_given_singleton + prob_testPositiveA_given_B * (1 - prob_A_given_singleton)

prob_A_given_testPositiveA = prob_testPositiveA_given_A * prob_A_given_singleton / prob_testPositiveA

print(paste('2H4. 2) Considering the birth data (twins, then singleton), the probability of species A given a positive test is :', prob_A_given_testPositiveA))
