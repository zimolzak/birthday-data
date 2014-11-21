bdd = read.csv("~/Desktop/birthday_data/birthday data.csv", sep="|", colClasses=c("integer", "integer", "character", "character"))
bdd$Time = bdd$hour + (bdd$min / 60)
bdd$Length = nchar(bdd$message)
bdd$Typical = (substr(bdd$message, 1, 5) ==  "Happy")

count_chars = function(char, str) {
	length(grep(char, strsplit(str, "")[[1]]))
}

count_exclam = function(str){
	count_chars("!", str)
}

bdd$ExclamationPoints = apply(array(bdd$message), 1, count_exclam)

 #################################

require(ggplot2)

# univar
ggplot(data=bdd, aes(x=Length)) + geom_density() + scale_x_log10()
ggplot(data=bdd, aes(x=Time)) + geom_density()

# time series with some clustering
ggplot(data=bdd, aes(x=Time, y=Length)) + geom_density2d() + geom_point()

# categorical vs numeric

ggplot(data=bdd, aes(factor(ExclamationPoints), Time)) + geom_boxplot() + geom_jitter() + coord_flip()
ggplot(data=bdd, aes(factor(ExclamationPoints), Length)) + geom_boxplot() + geom_jitter()
ggplot(data=bdd, aes(factor(Typical), Length)) + geom_boxplot() + geom_jitter()
ggplot(data=bdd, aes(factor(Typical), Time)) + geom_boxplot() + geom_jitter()

kruskal.test(bdd$Time, factor(bdd$ExclamationPoints))
kruskal.test(bdd$Length, factor(bdd$ExclamationPoints))
kruskal.test(bdd$Time, factor(bdd$Typical))
kruskal.test(bdd$Length, factor(bdd$Typical)) # p = 0.0182. Ones that don't start with "Happy" are clearly longer.

# category vs category

table(bdd$Typical, bdd$ExclamationPoints)
fisher.test(bdd$Typical, bdd$ExclamationPoints) # p-value = 0.04682

# future thoughts: capitalization and presence of Birthday, whether it includes Andy, comma after birthday, category of person
