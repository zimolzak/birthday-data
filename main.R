bdd = read.csv("~/Desktop/birthday_data/birthday data.csv", sep="|", colClasses=c("integer", "integer", "character", "character"))
bdd$Time = bdd$hour + (bdd$min / 60)
bdd$Length = nchar(bdd$message)

count_chars = function(char, str) {
	length(grep(char, strsplit(str, "")[[1]]))
}

count_exclam = function(str){
	count_chars("!", str)
}

bdd$ExclamationPoints = apply(array(bdd$message), 1, count_exclam)
