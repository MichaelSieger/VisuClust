pkgname <- "LinkageMaps"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('LinkageMaps')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("fuzzyColorPlot")
### * fuzzyColorPlot

flush(stderr()); flush(stdout())

### Name: fuzzyColorPlot
### Title: Displays a fuzzy color plot
### Aliases: fuzzyColorPlot

### ** Examples


# The example data
MD <- read.table(text="
Row  Name              Fett   Eiweiss Laktose Asche
  1  Mensch                  3,8      1,0       7,0      0,2
  2  Orang-Utan              3,5      1,5       6,0      0,2
  3  Schimpanse              3,7      1,2       7,0      0,2
  4  Zwergmeerkatze          2,9      2,1       7,2      0,3
  5  Pavian                  5,0      1,6       7,3      0,3
  6  Tamarin                 3,1      3,8       5,8      0,4
  7  Esel                    1,4      2,0       7,4      0,5
  8  Hauspferd               1,9      2,5       6,2      0,5
  9  Wildpferd               2,2      2,0       6,1      0,4
 10  Zebra                   2,1      2,3       8,3      0,4
 11  Wildschwein             6,8      4,8       5,5      1,7
 12  Lama                    2,4      7,3       6,0      0,5
 13  Kamel                   5,4      3,9       5,1      0,7
 14  Dromedar                4,5      3,6       5,0      0,7
 15  Sikahirsch             19,0     12,4       3,4      1,4
 16  Rothirsch              19,7     10,6       2,6      1,4
 17  Ren                    20,0      9,5       2,6      1,4
 18  Edmigazelle            19,0     12,4       3,3      1,5
 19  Thompson-Gazelle       19,6     10,5       2,7      1,4
 20  Schwarzfersenantilope  20,4     10,8       2,4      1,4
 21  Hausrind                3,7      3,4       4,8      0,7
 22  Zebu                    4,7      3,2       4,9      0,7
 23  Yak                     6,5      5,8       4,6      0,9
 24  Wasserbueffel            7,4      3,8       4,8      0,8
 25  Bison                   3,5      4,5       5,1      0,8
 26  Moschusochse            5,4      5,3       4,1      1,1
 27  Hausziege               4,5      2,9       4,1      0,8
 28  Hausschaf               7,4      5,5       4,8      1,0
 29  Haushund               12,9      7,9       3,1      1,2
 30  Wolf                    9,6      9,2       3,4      1,2
 31  Kojote                 10,7      9,9       3,0      0,9
 32  Schakal                10,5     10,0       3,0      1,2
 33  Afrikan.Wildhund        9,5      9,3       3,5      1,3
 34  Schwarzbaer             24,5     14,5       0,4      1,8
 35  Grizzly-Baer            22,3     11,1       0,6      1,5
 36  Braunbaer               22,6      7,9       2,1      1,4
 37  Eisbaer                 33,1     10,9       0,3      1,4
 38  Noerdlicher-Seebaer      53,3      8,9       0,1      0,5
 39  Biber                  11,7      8,1       2,6      1,1
 40  Goldhamster             4,9      9,4       4,9      1,4
 41  Wanderratte            10,3      8,4       2,6      1,3
 42  Hausmaus               13,1      9,0       3,0      1,3
 43  Hauskaninchen          18,3     13,9       2,1      1,8
 44  Florida-Waldkaninchen  13,9     23,7       1,7      1,5
 45  Wildkaninchen          17,9     12,5       1,0      2,0
 46  Blauwal                42,3     10,9       1,3      1,4
 47  Finnwal                32,4     17,8       0,3      1,0
 48  Buckelwal              33,0     12,5       1,1      1,6
 49  Delphin                33,0      6,8       1,1      0,7
 50  Braunbrustigel         10,1      7,2       2,0      2,3
", dec=",", header=TRUE)

library(cluster)
library(MASS)

D <- as.matrix(dist(MD[3:6]))
HK <- princomp(MD[3:6], cor=FALSE, scores=TRUE)
clustering = fanny(MD[3:6], 5)
S <- sammon(D, HK$scores[,1:2])

# A very basic example showing the default colors an symbols
fuzzyColorPlot(MD[3:6], 5, S$points)

# Custom symbols 
fuzzyColorPlot(MD[3:6], 5, S$points, clusterSymbols=c("a","b", "c","d", "e"))

# A Black-White Plot
fuzzyColorPlot(MD[3:6], 5, S$points, clusterColors=rep("black", 5), clusterSymbols=15:19)




cleanEx()
nameEx("linkmap")
### * linkmap

flush(stderr()); flush(stdout())

### Name: linkmap
### Title: Displays a linkage map
### Aliases: linkmap

### ** Examples


library(MASS)

MD <- read.table(text="
Row  Name             Fett Eiweiss Laktose Asche
  1  Mensch                  3,8      1,0       7,0      0,2
  2  Orang-Utan              3,5      1,5       6,0      0,2
  3  Schimpanse              3,7      1,2       7,0      0,2
  5  Pavian                  5,0      1,6       7,3      0,3
  7  Esel                    1,4      2,0       7,4      0,5
  8  Pferd                   1,9      2,5       6,2      0,5
 13  Kamel                   5,4      3,9       5,1      0,7
 21  Rind                    3,7      3,4       4,8      0,7
 27  Ziege                   4,5      2,9       4,1      0,8
 28  Schaf                   7,4      5,5       4,8      1,0
 29  Hund                   12,9      7,9       3,1      1,2
 30  Wolf                    9,6      9,2       3,4      1,2
 32  Schakal                10,5     10,0       3,0      1,2
 36  Braunbaer               22,6      7,9       2,1      1,4
 37  Eisbaer                 33,1     10,9       0,3      1,4
 39  Biber                  11,7      8,1       2,6      1,1
 41  Ratte                  10,3      8,4       2,6      1,3
 42  Maus                   13,1      9,0       3,0      1,3
 48  Buckelwal              33,0     12,5       1,1      1,6
 49  Delphin                33,0      6,8       1,1      0,7
 
", dec=",", header=TRUE)

D <- as.matrix(dist(MD[3:6]))
HK <- princomp(MD[3:6], cor=FALSE, scores=TRUE)
S <- sammon(D, HK$scores[,1:2])
k <- kmeans(D,center=5)
linkmap(S$points, D, cluster=k$cluster,main="Milch")




### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
