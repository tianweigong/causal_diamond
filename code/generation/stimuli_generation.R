#' ---
#' title: Time & Prevention <br> Stimuli Generation
#' author: Tia Gong
#' output:
#'    html_document:
#'      toc: true
#'      toc_depth: 3
#'      toc_float: true
#'      theme: default
#'      highlight: tango
#' ---

#+ General settings, echo = FALSE, results = 'hide' -------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)

#+ load packages -------------------
#' # load packages
library(tidyr)
library(dplyr)
library(ggplot2)
# rm(list=ls())

#+ test parameters -------------------
#' # test parameters
data.frame(Time = seq(0, 10, length.out = 1000)) %>%
  mutate(exp = dgamma(Time, shape = 36, rate = 12)) %>%
  gather(fun, Probability, c(exp)) %>%
  mutate(Form = factor(fun, levels = c('exp'),
                       labels = c('demo'))) %>%
  ggplot(aes(x = Time, y = Probability)) +
  geom_vline(xintercept = c(3.5)) +
  annotate(
    "rect",
    xmin = 0,
    xmax = 3.5,
    ymin = 0,
    ymax = 0.8,
    alpha = 0.2,
    fill = "blue"
  ) +
  geom_point() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ggtitle('Examples of causal delays')

#+ set parameters -------------------
#' # set parameters
#' #### BASIC IDEAS: <br>
#' 1. E's base rate: reliable,var=0.25 <br>
#' 2. A, B's base rates: not reliable but not totally memoryless, m=6, var=25 <br>
#' 3. The preventative cause in this experiments is in broad scope,
#' which can prevent E's activation within its effective time range despite the source<br>
#' 4. The preventative effect mean: 3s; the generative effect mean: 2s;
#' both are reliable, var= 0.25 (we can increase if it is too easy)<br>
#' 5. Contingency = 100%
#' 6. Each trial begin with base-rate E, in order to remind the exist of baserate
#' and help participants calculate the baserate activation.
#' 7. cut trials at 20s.
#'
#' #### NOTES: <br>
#' To be specific, the preventative cause prevent the ACTIVATION,
#' rather than the mechanism between generative causes and the effect.
#' For example, the case B-A------E where B->E is possible as long as A->E < AE,
#' but not very probable under our current parameter setting (we can negotiate it later).

k_e = 100
r_e = 20 #e->e: m=5,var=0.25
k_a = 1
r_a = 0.17 #a: m=6,var=25
k_b = 1
r_b = 0.17 #b:m=6,var=25

s_e = 5
s_a = 3
s_b = 3

#we use "ae" to represent preventative, while "be" to represent generative
#sinse "preventative_A_generative_B" is our first model :)
k_ae = 36
r_ae = 12 #preventative a->e: m=3,var=0.25
k_be = 9
r_be = 6  #generative b->e: m=1.5,var=0.25

E = E_raw = rgamma(s_e, shape = k_e, rate = r_e) %>% cumsum()
a_count = 0
b_count = 0
while (1) {
  A = rgamma(s_a, shape = k_a, rate = r_a)
  if (min(A) > 1 &&
      max(A) < 12) {
    # A causes cannot get too close to each other
    A = A %>% cumsum()
    if (max(A) < 20) {
      break
    } else{
      a_count = a_count + 1
    }
  } else{
    a_count = a_count + 1
  }
}

while (1) {
  B = rgamma(s_b, shape = k_b, rate = r_b)
  if (min(B) > 1 &&
      max(B) < 12) {
    # B causes cannot get too close to each other
    B = B %>% cumsum()
    if (max(B) < 20) {
      break
    } else{
      b_count = b_count + 1
    }
  } else{
    b_count = b_count + 1
  }
}
#+ preventative_A & generative_B -------------------
#' # preventative_A & generative_B
G = c()
for (i in 1:s_b) {
  G = c(G, rgamma(1, shape = (k_be), rate = r_be) + B[i])
}

E_cb = c(E, G)
for (i in 1:s_a) {
  blocktime = rgamma(1, shape = (k_ae), rate = r_ae) + A[i]
  E_cb = E_cb[E_cb > blocktime | E_cb <= A[i]]
}

pAgB = data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), rep("E", length(E_cb))), time =
                    c(A, B, E_cb))
initial = data.frame(obj = "E", time = 0) # set E activation as initialization
pAgB = rbind(pAgB, initial)
pAgB = pAgB[order(pAgB$time), ]

pAgB = pAgB[pAgB$time < 20, ]

write.csv(pAgB, "pAgB.csv", row.names = F, quote = F)
save(pAgB, file = "pAgB.Rda")

#+  generative_A & preventative_B -------------------
#' # generative_A & preventative_B
G = c()
for (i in 1:s_a) {
  G = c(G, rgamma(1, shape = (k_be), rate = r_be) + A[i])
}

E_cb = c(E, G)
for (i in 1:s_b) {
  blocktime = rgamma(1, shape = (k_ae), rate = r_ae) + B[i]
  E_cb = E_cb[E_cb > blocktime | E_cb <= B[i]]
}

gApB = data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), rep("E", length(E_cb))), time =
                    c(A, B, E_cb))
initial = data.frame(obj = "E", time = 0) # set E activation as initialization
gApB = rbind(gApB, initial)
gApB = gApB[order(gApB$time), ]

gApB = gApB[gApB$time < 20, ]

write.csv(gApB, "gApB.csv", row.names = F, quote = F)
save(gApB, file = "gApB.Rda")

#+ both generative -------------------
#' # both generative
G_b = c()

for (i in 1:s_b) {
  G_b = c(G_b, rgamma(1, shape = (k_be), rate = r_be) + B[i])
}
G_a = c()

for (i in 1:s_a) {
  G_a = c(G_a, rgamma(1, shape = (k_be), rate = r_be) + A[i])
}

E_cb = c(E, G_b, G_a)

gAgB = data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), rep("E", length(E_cb))), time =
                    c(A, B, E_cb))
initial = data.frame(obj = "E", time = 0) # set E activation as initialization
gAgB = rbind(gAgB, initial)
gAgB = gAgB[order(gAgB$time), ]

gAgB = gAgB[gAgB$time < 20, ]

write.csv(gAgB, "gAgB.csv", row.names = F, quote = F)
save(gAgB, file = "gAgB.Rda")

#+ both preventative -------------------
#' # both preventative
E_cb = E
for (i in 1:s_a) {
  blocktime = rgamma(1, shape = (k_ae), rate = r_ae) + A[i]
  E_cb = E_cb[E_cb > blocktime | E_cb <= A[i]]
}

for (i in 1:s_b) {
  blocktime = rgamma(1, shape = (k_ae), rate = r_ae) + B[i]
  E_cb = E_cb[E_cb > blocktime | E_cb <= B[i]]
}

pApB = data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), rep("E", length(E_cb))), time =
                    c(A, B, E_cb))
initial = data.frame(obj = "E", time = 0) # set E activation as initialization
pApB = rbind(pApB, initial)
pApB = pApB[order(pApB$time), ]

pApB = pApB[pApB$time < 20, ]

write.csv(pApB, "pApB.csv", row.names = F, quote = F)
save(pApB, file = "pApB.Rda")

#+ both noncausal -------------------
#' # both noncausal
E_cb = E

nAnB = data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), rep("E", length(E_cb))), time =
                    c(A, B, E_cb))
initial = data.frame(obj = "E", time = 0) # set E activation as initialization
nAnB = rbind(nAnB, initial)
nAnB = nAnB[order(nAnB$time), ]
nAnB = nAnB[nAnB$time < 20, ]

write.csv(nAnB, "nAnB.csv", row.names = F, quote = F)
save(nAnB, file = "nAnB.Rda")

#+ noncausal_A & generative_B -------------------
#' # noncausal_A & generative_B
G = c()
for (i in 1:s_b) {
  G = c(G, rgamma(1, shape = (k_be), rate = r_be) + B[i])
}

E_cb = c(E, G)

nAgB = data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), rep("E", length(E_cb))), time =
                    c(A, B, E_cb))
initial = data.frame(obj = "E", time = 0) # set E activation as initialization
nAgB = rbind(nAgB, initial)
nAgB = nAgB[order(nAgB$time), ]
nAgB = nAgB[nAgB$time < 20, ]

write.csv(nAgB, "nAgB.csv", row.names = F, quote = F)
save(nAgB, file = "nAgB.Rda")

#+ generative_A & noncausal_B -------------------
#' # generative_A & noncausal_B
G = c()
for (i in 1:s_a) {
  G = c(G, rgamma(1, shape = (k_be), rate = r_be) + A[i])
}

E_cb = c(E, G)

gAnB = data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), rep("E", length(E_cb))), time =
                    c(A, B, E_cb))
initial = data.frame(obj = "E", time = 0) # set E activation as initialization
gAnB = rbind(gAnB, initial)
gAnB = gAnB[order(gAnB$time), ]
gAnB = gAnB[gAnB$time < 20, ]

write.csv(gAnB, "gAnB.csv", row.names = F, quote = F)
save(gAnB, file = "gAnB.Rda")

#+ noncausal_A & preventative_B -------------------
#' # noncausal_A & preventative_B
E_cb = E

for (i in 1:s_b) {
  blocktime = rgamma(1, shape = (k_ae), rate = r_ae) + B[i]
  E_cb = E_cb[E_cb > blocktime | E_cb <= B[i]]
}

nApB = data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), rep("E", length(E_cb))), time =
                    c(A, B, E_cb))
initial = data.frame(obj = "E", time = 0) # set E activation as initialization
nApB = rbind(nApB, initial)
nApB = nApB[order(nApB$time), ]
nApB = nApB[nApB$time < 20, ]

write.csv(nApB, "nApB.csv", row.names = F, quote = F)
save(nApB, file = "nApB.Rda")

#+ preventative_A & noncausal_B -------------------
#' # preventative_A & noncausal_B
E_cb = E

for (i in 1:s_a) {
  blocktime = rgamma(1, shape = (k_ae), rate = r_ae) + A[i]
  E_cb = E_cb[E_cb > blocktime | E_cb <= A[i]]
}

pAnB = data.frame(obj = c(rep("A", length(A)), rep("B", length(B)), rep("E", length(E_cb))), time =
                    c(A, B, E_cb))
initial = data.frame(obj = "E", time = 0) # set E activation as initialization
pAnB = rbind(pAnB, initial)
pAnB = pAnB[order(pAnB$time), ]
pAnB = pAnB[pAnB$time < 20, ]

write.csv(pAnB, "pAnB.csv", row.names = F, quote = F)
save(pAnB, file = "pAnB.Rda")