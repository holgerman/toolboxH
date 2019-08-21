theme_peter = function(beschriftungszahlengroesse = 20, relfak = 2) {
  theme_peter = theme_grey() + theme(axis.text.x = element_text(angle=0, hjust=0, size = beschriftungszahlengroesse), axis.text.y = element_text(size = beschriftungszahlengroesse), axis.title= element_text(size = beschriftungszahlengroesse), legend.title=element_text(size = rel(relfak)), legend.text=element_text(size = rel(relfak)), strip.text = element_text(face="bold", size=rel(relfak)))
  theme_peter
}
