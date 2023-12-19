# Установить и загрузить библиотеку tidyverse, если она еще не установлена
if (!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}

# 1. Загрузить файл *.csv с вашего компьютера в R
# Замените "file.csv" на имя вашего файла
data <- read_csv("C:\\Osnova\\Education\\3_god_1_semestr\\DS\\DS_Lab_1\\Electric_Vehicle_Population_Data.csv")

# 2. Вывести первые строки набора данных
head(data)

# 3. Проверить уровни столбца "Country" и вывести график
levels_county <- unique(data$County)
barplot(table(data$County), main = "Уровни столбца 'Country'", xlab = "County", ylab = "Количество")

# 4. Проверить уровни столбца "City" и вывести график
levels_city <- unique(data$City)
barplot(table(data$City), main = "Уровни столбца 'City'", xlab = "City", ylab = "Количество")

# 5. Проверить уровни столбца "State" и вывести график
levels_state <- unique(data$State)
barplot(table(data$State), main = "Уровни столбца 'State'", xlab = "State", ylab = "Количество")

# 6. Проверить уровни столбца "Model Year" и вывести график
levels_model_year <- unique(data$`Model Year`)
barplot(table(data$`Model Year`), main = "Уровни столбца 'Model Year'", xlab = "Model Year", ylab = "Количество")

# 7. Проверить уровни столбца "Make" и вывести график
levels_make <- unique(data$Make)
barplot(table(data$Make), main = "Уровни столбца 'Make'", xlab = "Make", ylab = "Количество")

# 10. Вывести 5 самых больших категорий в столбце "Make"
top_make <- head(sort(table(data$Make), decreasing = TRUE), 5)
print(top_make)

# 9. Вывести 5 самых больших категорий в столбце "Make" с использованием ggplot2
top_make <- data %>%
  group_by(Make) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(5)

ggplot(top_make, aes(x = fct_reorder(Make, count), y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "5 самых больших категорий в столбце 'Make'", x = "Make", y = "Количество")


# 8. Проверить уровни столбца "Model" и вывести график
levels_model <- unique(data$Model)
barplot(table(data$Model), main = "Уровни столбца 'Model'", xlab = "Model", ylab = "Количество")

# 9. Проверить уровни столбца "Electric Vehicle Type" и вывести график
levels_ev_type <- unique(data$`Electric Vehicle Type`)
barplot(table(data$`Electric Vehicle Type`), main = "Уровни столбца 'Electric Vehicle Type'", xlab = "Electric Vehicle Type", ylab = "Количество")

# Упорядочим марки по количеству автомобилей
top_makes <- data %>%
  count(Make) %>%
  top_n(10) %>%
  arrange(desc(n)) %>%
  pull(Make)


# Выберем только данные для выбранных 10 популярных марок
filtered_data <- data %>%
  filter(`Make` %in% top_makes)

# Построим круговую диаграмму для типов электромобилей этих 10 популярных марок
ggplot(filtered_data, aes(x = "", fill = `Electric Vehicle Type`)) +
  geom_bar(width = 1, position = "fill") +
  coord_polar("y", start = 0) +
  facet_wrap(~ Make) +
  labs(title = "Типы электромобилей для 10 популярных марок", x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Установка и загрузка необходимых пакетов
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}

# Создание Boxplot для года выпуска по каждому производителю
ggplot(data, aes(x = Make, y = `Model Year`)) +
  geom_boxplot() +
  labs(title = "Boxplot для года выпуска по производителям",
       x = "Производитель", y = "Год выпуска") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Выберем только данные для выбранных 10 популярных производителей
top_makes <- data %>%
  count(Make) %>%
  top_n(10) %>%
  arrange(desc(n)) %>%
  pull(Make)

filtered_data <- data %>%
  filter(Make %in% top_makes)

# Создание Boxplot для года выпуска по каждому из 10 популярных производителей
ggplot(filtered_data, aes(x = Make, y = `Model Year`)) +
  geom_boxplot() +
  labs(title = "Boxplot для года выпуска по 10 популярным производителям",
       x = "Производитель", y = "Год выпуска") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Выберем только данные для выбранных 10 популярных производителей и с годом выпуска не раньше 2013
top_makes <- data %>%
  count(Make) %>%
  top_n(10) %>%
  arrange(desc(n)) %>%
  pull(Make)

filtered_data <- data %>%
  filter(Make %in% top_makes, `Model Year` >= 2013)

# Создание Boxplot для года выпуска по каждому из 10 популярных производителей и начиная с 2013 года
ggplot(filtered_data, aes(x = Make, y = `Model Year`)) +
  geom_boxplot() +
  labs(title = "Boxplot для года выпуска по 10 популярным производителям (начиная с 2013 года)",
       x = "Производитель", y = "Год выпуска") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Загрузка необходимых пакетов
library(tidyverse)

# Подготовка данных (в данном случае - фильтрация и преобразование категориальных переменных)
data_filtered <- data %>%
  filter(!is.na(`Model Year`), !is.na(`Electric Range`)) %>%
  select(`Electric Range`, Make, Model, `Model Year`) %>%
  mutate(
    Make = as.factor(Make),
    Model = as.factor(Model)
  )

# Построение модели линейной регрессии
linear_model <- lm(`Electric Range` ~ Make + Model + `Model Year`, data = data_filtered)

# Проверим сводную информацию о модели
summary(linear_model)

# Создание dataframe с предсказанными и фактическими значениями
predictions <- data_filtered %>%
  mutate(predicted = predict(linear_model))

# График фактических и предсказанных значений Electric Range
ggplot(predictions, aes(x = `Electric Range`, y = predicted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Фактические vs. Предсказанные значения Electric Range",
    x = "Фактические значения",
    y = "Предсказанные значения"
  )

# Предсказанные значения
predictions <- data_filtered %>%
  mutate(predicted = predict(linear_model))

# График фактических значений Electric Range
ggplot(data_filtered, aes(x = `Electric Range`)) +
  geom_histogram(binwidth = 20, fill = "lightgreen", color = "black") +
  labs(
    title = "Гистограмма фактических значений Electric Range",
    x = "Фактические значения",
    y = "Частота"
  )

# График предсказанных значений Electric Range
ggplot(predictions, aes(x = predicted)) +
  geom_histogram(binwidth = 20, fill = "lightgreen", color = "black") +
  labs(
    title = "Гистограмма предсказанных значений Electric Range",
    x = "Предсказанные значения",
    y = "Частота"
  )


# График зависимости Electric Range от Model Year
ggplot(data_filtered, aes(x = `Model Year`, y = `Electric Range`)) +
  geom_point() +
  labs(
    title = "Зависимость Electric Range от Model Year",
    x = "Model Year",
    y = "Electric Range"
  )

# График зависимости Electric Range от Make с учетом Model Year
ggplot(data_filtered, aes(x = Make, y = `Electric Range`, color = `Model Year`)) +
  geom_point() +
  labs(
    title = "Зависимость Electric Range от Make с учетом Model Year",
    x = "Make",
    y = "Electric Range"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# График зависимости Electric Range от Model с учетом Make
ggplot(data_filtered, aes(x = Model, y = `Electric Range`, fill = Make)) +
  geom_boxplot() +
  labs(
    title = "Зависимость Electric Range от Model с учетом Make",
    x = "Model",
    y = "Electric Range"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Выбираем только 10 самых популярных автомобилей
top_10_cars <- data_filtered %>%
  count(Model) %>%
  top_n(10) %>%
  pull(Model)

# Оставляем только данные для этих 10 автомобилей
top_10_data <- data_filtered %>%
  filter(Model %in% top_10_cars)

# Создаем график зависимости Electric Range от Model с учетом Make для 10 популярных автомобилей
ggplot(top_10_data, aes(x = Model, y = `Electric Range`, fill = Make)) +
  geom_boxplot() +
  labs(
    title = "Зависимость Electric Range от Model с учетом Make (10 популярных автомобилей)",
    x = "Model",
    y = "Electric Range"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(predictions, aes(x = `Electric Range`, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(
    title = "Фактические vs. Предсказанные значения Electric Range",
    x = "Фактические значения",
    y = "Предсказанные значения"
  )

# Создание нового dataframe с уникальными значениями Model Year, Make и Model
unique_combinations <- data_filtered %>%
  distinct(`Model Year`, Make, Model)

# Предсказание Electric Range на основе Model Year, Make и Model
unique_combinations$predicted_range <- predict(linear_model, newdata = unique_combinations)

# График зависимости предсказанных значений Electric Range от Model Year
ggplot(unique_combinations, aes(x = `Model Year`, y = predicted_range)) +
  geom_line(color = "blue") +
  geom_point(data = data_filtered, aes(x = `Model Year`, y = `Electric Range`), alpha = 0.5) +
  labs(
    title = "Предсказанные значения Electric Range от Model Year",
    x = "Model Year",
    y = "Предсказанные значения Electric Range"
  )

predictions$error <- predictions$predicted - predictions$`Electric Range`

ggplot(predictions, aes(x = error)) +
  geom_histogram(binwidth = 20, fill = "skyblue", color = "black") +
  labs(
    title = "Гистограмма ошибок предсказания Electric Range",
    x = "Ошибка предсказания",
    y = "Частота"
  )

ggplot(predictions, aes(x = Make, y = error, fill = Model)) +
  geom_boxplot() +
  labs(
    title = "Ошибки предсказания Electric Range по категориям Make и Model",
    x = "Make",
    y = "Ошибка предсказания"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(predictions, aes(x = seq_along(predictions$`Electric Range`), y = predictions$`Electric Range` - predictions$predicted)) +
  geom_line() +
  labs(
    title = "Линейная диаграмма остатков предсказания Electric Range",
    x = "Порядок предсказания",
    y = "Остаток"
  )

# Предсказания модели
predicted_values <- predict(linear_model, data_filtered)

# R-квадрат
r_squared <- summary(linear_model)$r.squared

# Среднеквадратическая ошибка (MSE)
mse <- mean((predicted_values - data_filtered$`Electric Range`)^2)

# Средняя абсолютная ошибка (MAE)
mae <- mean(abs(predicted_values - data_filtered$`Electric Range`))

# Вывод результатов
cat("R-квадрат:", r_squared, "\n")
cat("Среднеквадратическая ошибка (MSE):", mse, "\n")
cat("Средняя абсолютная ошибка (MAE):", mae, "\n")


















