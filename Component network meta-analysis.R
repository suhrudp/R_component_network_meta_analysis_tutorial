# COMPONENT NETWORK META-ANALYSIS

## **LOAD LIBRARIES**

```{r}
library(netmeta)
library(meta)
library(metafor)
```

## **ATTACH DATA**

```{r}
df <- read.csv(file.choose())
attach(df)
View(df)
```

## CALCULATE EFFECT SIZE

```{r}
dfes <- escalc(measure="RR", ai=T1pos, bi=T1neg, ci=T2pos, di=T2neg, data=df)
View(dfes)
sei <- sqrt(dfes$vi)
dfes <- cbind(dfes, sei)
```

## RANDOM EFFECTS META-ANALYSIS

```{r}
ma.mod <- rma(yi, vi, data=dfes)
summary(ma.mod)
forest(ma.mod, slab=Author)
```

## COMPONENT NETWORK META-ANALYSIS

```{r}
dfes$Treat1 <- as.factor(dfes$Treat1)
dfes$Treat2 <- as.factor(dfes$Treat2)

nma.mod <- netmeta(data=dfes, TE=yi, seTE=sei, treat1=Treat1, treat2=Treat2, studlab=Author, sm="RR", small.values="good", reference.group="Placebo", all.treatments=T, details.chkmultiarm=T, sep.trts='+')
summary(nma.mod)
nma.rank <- netrank(nma.mod)
nma.rank
nma.split <- netsplit(nma.mod)
nma.split
nma.pair <- netpairwise(nma.mod)
nma.pair
forest(nma.mod)
forest(nma.pair)
plot(nma.split)
plot(nma.rank)
netgraph(nma.mod)
netheat(nma.mod)

nma.comb <- netcomb(nma.mod, sep.comps = "+", inactive = "Placebo")

summary(nma.comb)
forest(nma.comb)

netgraph(nma.comb, lwd=2)

nmacombcomparison <- netcomparison(nma.comb)
summary(nmacombcomparison)
forest(nmacombcomparison)

nmacombrank <- netrank(nma.comb, small.values = "good")
nmacombrank
plot(nmacombrank, nchar.trts = 15)

nmacombconnect <- netconnection(data = dfes, treat1 = Treat1, treat2 = Treat2, studlab = Author, sep.trts = "+")
summary(nmacombconnect)
netgraph(nmacombconnect, lwd=1.5)
```
