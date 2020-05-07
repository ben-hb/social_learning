# Rational Social Learning Simulations

**Introduction**

Consider an agent deciding which restaurant to go to: restauarant A or restaurant B. The agent knows that one restaurant is good and the other is bad but is uncertain about which is which. 

To help make their decision, the agent receives two pieces of information. First, the agent receives a private signal correlating with which restaurant is better. Then, the agent observes which restaurant each agent before them decided to go to. The agent infers the likely better state, acts accordingly, and then play passes to the other agent. 

This simulation calculates, for a given private signal and permutation of observed actions, the optimal decision for each agent to make. 

**Goals**

We seek to investigate the implications of social learnings setups which can't easily be solved analytically. 

In particular, this tool is used to explore: 

1. Immitation and anti-immitation 
2. Herding speed 
3. Herding accuracy 
4. Limited observation 
5. Orderless observation 
6. Asymmetric signal predictiveness 

**Technical**

I use a simulation structure to estimate rational beliefs of states given signals and action sequences. For the 20th agent in line to generate their belief after observing the first 19 actions, they consider one million simulations of the signal generation and actions before them if state = A and one million simulations if state = B. They then assess the frequency of the observed action sequence in each state and employ Bayesian updating to generate their beliefs accordingly. In order to simulate the actions of the 19th agent, however, the 19th agent must simulate one million simulations of the first 18 actions under both state A and state B. The 18th agent must do the same, and so on, until the optimal actions of all agents are known. 

An iteration is considered to have herded if the next agent's optimal action can be perfectly predicted without knowledge of the agent's private signal. 

**About**

This research project is in collaboration with [Matthew Rabin]("https://scholar.harvard.edu/rabin") of Harvard University and is based on much of his prior research with [Erik Eyster]("http://econ.ucsb.edu/people/faculty/erik-eyster") of UCSB. 