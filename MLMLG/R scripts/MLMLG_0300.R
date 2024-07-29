matriculados = personas matriculadas
tipo = colegio (C) o universidad (U)
nv = número de delitos violentos cometidos en la universidad o colegio
nvrate = número de delitos violentos por cada 1000 estudiantes
matriculados = personas matriculadas, en miles
region = región del país (C = Central, MO = Medio Oeste, NE = Noreste, SE = Sudeste, SO = Suroeste y O = Oeste)

# Create a bar plot of the number of violent crimes (nv)
ggplot(data, aes(x = factor(region), y = nv, fill = tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Violent Crimes by Region and Type",
       x = "Region",
       y = "Number of Violent Crimes") +
  theme_minimal()