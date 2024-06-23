#https://bwlewis.github.io/rthreejs/index.html

library(threejs)
data(LeMis)
LeMis
graphjs(LeMis, layout=layout_with_fr(LeMis, dim=3))


if(!require("devtools")) install.packages("devtools")
devtools::install_github("bwlewis/rthreejs")

library(threejs)
z <- seq(-10, 10, 0.1)
x <- cos(z)
y <- sin(z)
scatterplot3js(x, y, z, size = 0.1,color=rainbow(length(z)))


data(ego)
graphjs(ego, bg="black")


## 지구위 도시 인구 
install.packages("maps")
runApp(system.file("examples/globe", package="threejs"))


##아래 코드의 첫 번째 비트는 클러스터, 정점 페이지 순위 값 및 각 클러스터의 가장 중요한 정점을 계산합니다. 클러스터 구성원별로 꼭짓점의 색상을 지정하지만 시각화에 표시되지 않도록 각 클러스터의 가장 중요한 꼭짓점을 제외한 모든 꼭짓점을 투명하게 설정합니다. 마지막으로 코드는 각 꼭지점의 꼭지점 좌표를 해당 클러스터의 가장 중앙 꼭지점과 동일하게 명시적으로 설정합니다.
library(threejs)
data(LeMis)
N  <- length(V(LeMis))

# Vertex page rank values
pr <- page_rank(LeMis)$vector
# order the page rank values
i <- order(pr, decreasing=TRUE)

# Vertex cluster membership
cl <- unclass(membership(cluster_louvain(LeMis)))

# Find the index of the highest page rank vertex in each cluster
idx <- aggregate(seq(1:N)[i], by=list(cl[i]), FUN=head, 1)$x
# Create a default force-directed layout for the whole network
l1 <- norm_coords(layout_with_fr(LeMis, dim=3))
# Collapse the layout to just the idx vertices
l0 <- Reduce(rbind,Map(function(i) l1[idx[i],], cl))

# Grouped vertex colors, setting all but idx vertices transparent
col <- rainbow(length(idx), alpha=0)[cl]
col[idx] <- rainbow(length(idx), alpha=1)
V(LeMis)$label[idx]


# animation layouts, one for each of the idx vertices, and
# animation color schemes, one scheme for each idx vertex
click <- Map(function(i)
{
  x <- l0
  x[cl == i, ] <- l1[cl == i, ]
  c <- col
  c[cl == i] <- rainbow(length(idx), alpha=1)[i]
  list(layout=x, vertex.color=c)
}, seq(idx))
names(click) <- paste(idx)
str(head(click, 2))


(graphjs(LeMis, layout=l0, click=click, vertex.color=col, fps=20, font.main="78px Arial"))


###

library("threejs")
earth <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg"
globejs(img=earth, bg="white")


# Approximate locations as factors
f <- flights()
dest   <- factor(sprintf("%.2f:%.2f", f[,3], f[,4]))

# A table of destination frequencies
freq <- sort(table(dest), decreasing=TRUE)

# The most frequent destinations in these data
frequent_destinations <- names(freq)[1:10]

# Subset the flight data by destination frequency
idx <- dest %in% frequent_destinations
frequent_flights <- f[idx, ]

# Lat/long and counts of frequent flights
ll <- unique(frequent_flights[, 3:4])

# Plot frequent destinations as bars, and the flights to and from
# them as arcs. Adjust arc width and color by frequency.
globejs(lat=ll[, 1], long=ll[, 2], arcs=frequent_flights,
        bodycolor="#aaaaff", arcsHeight=0.3, arcsLwd=2,
        arcsColor="#ffff00", arcsOpacity=0.15,
        atmosphere=TRUE, color="#00aaff", pointsize=0.5)