bdd = read.csv("~/Desktop/birthday_data/birthday data.csv", sep="|", colClasses=c("integer", "integer", "character", "character"))
bdd$Time = bdd$hour + (bdd$min / 60)
bdd$Length = nchar(bdd$message)
bdd$Starts_with_word_Happy = (substr(bdd$message, 1, 5) ==  "Happy")

count_chars = function(char, str) {
	length(grep(char, strsplit(str, "")[[1]]))
}

count_exclam = function(str){
	count_chars("!", str)
}

bdd$Number_of_exclamation_points = factor(apply(array(bdd$message), 1, count_exclam))

 #################################

require(ggplot2)

# univar
ggplot(data=bdd, aes(x=Length)) + geom_density() + scale_x_log10(limits=c(5,500))
ggplot(data=bdd, aes(x=Time)) + geom_density() + scale_x_continuous(limits=c(-3,24))

# time series with some clustering
ggplot(data=bdd, aes(x=Time, y=Length)) + geom_density2d() + geom_point()

# categorical vs numeric

ggplot(data=bdd, aes(Number_of_exclamation_points, Time)) + geom_boxplot() + geom_jitter() + coord_flip()
ggplot(data=bdd, aes(Number_of_exclamation_points, Length)) + geom_boxplot() + geom_jitter()
ggplot(data=bdd, aes(Starts_with_word_Happy, Length)) + geom_boxplot() + geom_jitter()
ggplot(data=bdd, aes(Starts_with_word_Happy, Time)) + geom_boxplot() + geom_jitter()

kruskal.test(bdd$Time, bdd$Number_of_exclamation_points)
kruskal.test(bdd$Length, bdd$ExclamationPoints)
kruskal.test(bdd$Time, bdd$Starts_with_word_Happy)
kruskal.test(bdd$Length, bdd$Starts_with_word_Happy) # p = 0.0182. Ones that don't start with "Happy" are clearly longer.

# category vs category

table(bdd$Typical, bdd$Number_of_exclamation_points)
fisher.test(bdd$Starts_with_word_Happy, bdd$Number_of_exclamation_points) # p-value = 0.04682

# future thoughts: capitalization and presence of Birthday, whether it includes Andy, comma after birthday, category of person
