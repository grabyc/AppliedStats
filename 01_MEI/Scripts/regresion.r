ej3 <- read.csv2( file="C:\\Users\\admin\\Documents\\EdadPesoGrasas.csv", header = TRUE)

gg <- ggplot(data=ej3, mapping=aes(edad, grasas, label = rownames(ej3)))

gg + 
  geom_point() +
  geom_text(hjust=0, nudge_x = 0.4)

reg <- lm(grasas ~ edad, data = ej3)

gg_reg <- ggplot(data=ej3, mapping=aes_string(x = names(ej3)[3], y=names(ej3)[4]))

gg_reg + 
  geom_point() +
  stat_smooth(method ="lm", se = F)

par(mfrow = c(2,2))
plot(reg)