# ============================================================================
# NYPD SHOOTING INCIDENTS - COMPLETE ANALYSIS IN R
# All plots from Python notebook converted to R
# Using: data.table, plotly, base R graphics
# Avoiding: tidyverse, sf, leaflet, gt, feasts
# ============================================================================

# Load required libraries
library(data.table)
library(plotly)
library(zoo)          # For rolling mean
library(geopandas)    # For spatial operations (if needed)

# Suppress warnings
options(warn = -1)

cat("Starting Enhanced Exploratory Data Analysis for NYPD Shooting Incidents...\n\n")

# ============================================================================
# SECTION 1: DATA LOADING AND PREPROCESSING
# ============================================================================

# Load dataset
tryCatch({
  df <- fread("NYPD_cleaned.csv", na.strings = c("", "NA", "(null)", "UNKNOWN"))
  cat(sprintf("Dataset loaded successfully. Shape: %d rows x %d columns\n\n", 
              nrow(df), ncol(df)))
}, error = function(e) {
  stop("Error: 'NYPD_cleaned.csv' not found")
})

# Display data info
cat("--- Initial Data Information ---\n")
cat(sprintf("Rows: %d\nColumns: %d\n\n", nrow(df), ncol(df)))

# Handle missing values in location columns
location_cols <- c('LOC_OF_OCCUR_DESC', 'LOC_CLASSFCTN_DESC', 'LOCATION_DESC')
for (col in location_cols) {
  if (col %in% names(df)) {
    df[[col]] <- ifelse(is.na(df[[col]]) | df[[col]] == "(null)", 
                        'UNKNOWN_LOCATION_TYPE', df[[col]])
  }
}

# Fill missing Jurisdiction Code with mode
if ('JURISDICTION_CODE' %in% names(df)) {
  mode_val <- names(sort(table(df$JURISDICTION_CODE), decreasing = TRUE))[1]
  df[is.na(JURISDICTION_CODE), JURISDICTION_CODE := mode_val]
  cat(sprintf("Filled missing JURISDICTION_CODE with mode: %s\n", mode_val))
}

# Function to clean age groups
clean_age_group <- function(age_str) {
  if (is.na(age_str) || age_str %in% c('U', '<18', '18-24', '25-44', '45-64', '65+')) {
    return(age_str)
  }
  
  age_num <- suppressWarnings(as.numeric(age_str))
  if (is.na(age_num)) return('U')
  
  if (age_num >= 0 && age_num <= 17) return('<18')
  else if (age_num >= 18 && age_num <= 24) return('18-24')
  else if (age_num >= 25 && age_num <= 44) return('25-44')
  else if (age_num >= 45 && age_num <= 64) return('45-64')
  else if (age_num >= 65) return('65+')
  else return('U')
}

# Clean demographic columns
demographic_cols <- c('PERP_AGE_GROUP', 'PERP_SEX', 'PERP_RACE', 
                      'VIC_AGE_GROUP', 'VIC_SEX', 'VIC_RACE')

for (col in demographic_cols) {
  if (col %in% names(df)) {
    df[[col]] <- gsub('UNKNOWN|N/A|\\(null\\)', 'U', df[[col]])
    df[[col]] <- gsub('ASIAN/PACIFIC ISLANDER', 'ASIAN', df[[col]])
    df[[col]] <- gsub('AMERICAN INDIAN/ALASKAN NATIVE', 'NATIVE AMERICAN', df[[col]])
    df[[col]] <- ifelse(is.na(df[[col]]), 'U', df[[col]])
    
    if (grepl('AGE_GROUP', col)) {
      df[[col]] <- sapply(df[[col]], clean_age_group)
    }
  }
}

# Parse datetime and extract temporal features
df[, OCCUR_DATETIME := as.POSIXct(OCCUR_DATETIME, format = "%Y-%m-%d %H:%M:%S")]
df[, `:=`(
  YEAR = as.integer(format(OCCUR_DATETIME, "%Y")),
  MONTH = format(OCCUR_DATETIME, "%B"),
  DAY_OF_WEEK = format(OCCUR_DATETIME, "%A"),
  HOUR = as.integer(format(OCCUR_DATETIME, "%H")),
  DATE = as.Date(OCCUR_DATETIME)
)]

min_year <- min(df$YEAR, na.rm = TRUE)
max_year <- max(df$YEAR, na.rm = TRUE)

cat("\n=== Data preprocessing complete ===\n\n")

# ============================================================================
# SECTION 2: TEMPORAL ANALYSIS PLOTS
# ============================================================================

cat("Generating temporal analysis plots...\n")

# ---- PLOT 1: Annual Trends with Rolling Mean ----
annual_counts <- df[, .N, by = YEAR][order(YEAR)]
setnames(annual_counts, "N", "Annual_Incidents")
annual_counts[, Rolling_Mean := rollmean(Annual_Incidents, k = 3, 
                                         fill = NA, align = "center")]

fig1 <- plot_ly(annual_counts) %>%
  add_trace(x = ~YEAR, y = ~Annual_Incidents, type = 'scatter', 
            mode = 'lines+markers', name = 'Annual Incidents',
            line = list(color = 'steelblue', width = 2)) %>%
  add_trace(x = ~YEAR, y = ~Rolling_Mean, type = 'scatter', 
            mode = 'lines+markers', name = '3-Year Rolling Mean',
            line = list(color = 'red', width = 2, dash = 'dash')) %>%
  layout(
    title = sprintf('Annual Number of Shooting Incidents with 3-Year Rolling Mean (%d-%d)', 
                    min_year, max_year),
    xaxis = list(title = 'Year', showspikes = TRUE),
    yaxis = list(title = 'Number of Incidents', showspikes = TRUE),
    hovermode = 'x unified',
    template = 'plotly_white'
  )
print(fig1)

# ---- PLOT 2: Monthly Distribution ----
month_order <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")
monthly_counts <- df[, .N, by = MONTH]
monthly_counts[, MONTH := factor(MONTH, levels = month_order)]
monthly_counts <- monthly_counts[order(MONTH)]

fig2 <- plot_ly(monthly_counts, x = ~MONTH, y = ~N, type = 'bar',
                marker = list(color = ~N, colorscale = 'Viridis', showscale = TRUE)) %>%
  layout(
    title = 'Monthly Distribution of Shooting Incidents',
    xaxis = list(title = 'Month', tickangle = 45),
    yaxis = list(title = 'Number of Incidents'),
    template = 'plotly_white'
  )
print(fig2)

# ---- PLOT 3: Hourly Distribution ----
hourly_counts <- df[, .N, by = HOUR][order(HOUR)]
all_hours <- data.table(HOUR = 0:23)
hourly_counts <- merge(all_hours, hourly_counts, by = "HOUR", all.x = TRUE)
hourly_counts[is.na(N), N := 0]

fig3 <- plot_ly(hourly_counts, x = ~HOUR, y = ~N, type = 'bar',
                marker = list(color = ~N, colorscale = 'Inferno', showscale = TRUE)) %>%
  layout(
    title = 'Diurnal Distribution of Shooting Incidents',
    xaxis = list(title = 'Hour of Day (24-hour format)', tickmode = 'linear', 
                 tick0 = 0, dtick = 1),
    yaxis = list(title = 'Number of Incidents'),
    template = 'plotly_white'
  )
print(fig3)

# ---- PLOT 4: Borough Distribution ----
borough_counts <- df[, .N, by = BORO][order(-N)]

fig4 <- plot_ly(borough_counts, x = ~BORO, y = ~N, type = 'bar',
                marker = list(color = ~N, colorscale = 'Viridis', showscale = TRUE)) %>%
  layout(
    title = 'Number of Shooting Incidents by Borough',
    xaxis = list(title = 'Borough'),
    yaxis = list(title = 'Number of Incidents'),
    template = 'plotly_white'
  )
print(fig4)

# ---- PLOT 5: Shooting Trends per Borough Over Time ----
# Filter valid NYC coordinates
nyc_lat_min <- 40.47; nyc_lat_max <- 40.92
nyc_lon_min <- -74.27; nyc_lon_max <- -73.69

df_geo <- df[Latitude >= nyc_lat_min & Latitude <= nyc_lat_max &
               Longitude >= nyc_lon_min & Longitude <= nyc_lon_max]

boro_yearly_trend <- df_geo[, .N, by = .(YEAR, BORO)][order(BORO, YEAR)]
setnames(boro_yearly_trend, "N", "Incidents")

fig5 <- plot_ly()
boroughs <- unique(boro_yearly_trend$BORO)
colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd')

for (i in seq_along(boroughs)) {
  boro_data <- boro_yearly_trend[BORO == boroughs[i]]
  fig5 <- fig5 %>%
    add_trace(x = boro_data$YEAR, y = boro_data$Incidents,
              type = 'scatter', mode = 'lines+markers',
              name = boroughs[i],
              line = list(color = colors[i], width = 2))
}

fig5 <- fig5 %>%
  layout(
    title = 'Shooting Trends per Borough Over Time',
    xaxis = list(title = 'Year'),
    yaxis = list(title = 'Number of Shooting Incidents'),
    template = 'plotly_white',
    hovermode = 'x unified'
  )
print(fig5)

# ============================================================================
# SECTION 3: DEMOGRAPHIC ANALYSIS PLOTS
# ============================================================================

cat("\nGenerating demographic analysis plots...\n")

# ---- PLOT 6: Perpetrator and Victim Age Group Distributions ----
age_order <- c('<18', '18-24', '25-44', '45-64', '65+')

perp_counts <- df[PERP_AGE_GROUP %in% age_order, .N, by = PERP_AGE_GROUP]
vic_counts <- df[VIC_AGE_GROUP %in% age_order, .N, by = VIC_AGE_GROUP]

# Ensure all age groups are present
perp_counts <- merge(data.table(PERP_AGE_GROUP = age_order), 
                     perp_counts, by = "PERP_AGE_GROUP", all.x = TRUE)
perp_counts[is.na(N), N := 0]
perp_counts[, PERP_AGE_GROUP := factor(PERP_AGE_GROUP, levels = age_order)]

vic_counts <- merge(data.table(VIC_AGE_GROUP = age_order), 
                    vic_counts, by = "VIC_AGE_GROUP", all.x = TRUE)
vic_counts[is.na(N), N := 0]
vic_counts[, VIC_AGE_GROUP := factor(VIC_AGE_GROUP, levels = age_order)]

fig6 <- plot_ly() %>%
  add_trace(x = age_order, y = perp_counts$N, type = 'bar',
            name = 'Perpetrators', marker = list(color = 'steelblue')) %>%
  add_trace(x = age_order, y = vic_counts$N, type = 'bar',
            name = 'Victims', marker = list(color = 'seagreen')) %>%
  layout(
    title = 'Perpetrator and Victim Age Group Distributions',
    xaxis = list(title = 'Age Group'),
    yaxis = list(title = 'Number of Incidents'),
    barmode = 'group',
    template = 'plotly_white'
  )
print(fig6)

# ---- PLOT 7: Age Group Cross-Tabulation Heatmap (Base R) ----
df_age_groups <- df[PERP_AGE_GROUP %in% age_order & VIC_AGE_GROUP %in% age_order]
age_crosstab <- table(
  factor(df_age_groups$PERP_AGE_GROUP, levels = age_order),
  factor(df_age_groups$VIC_AGE_GROUP, levels = age_order)
)

png("age_crosstab_heatmap.png", width = 800, height = 600)
par(mar = c(5, 5, 4, 2))
image(1:5, 1:5, age_crosstab, 
      col = colorRampPalette(c("white", "yellow", "green", "blue"))(20),
      xlab = "Victim Age Group", ylab = "Perpetrator Age Group",
      main = "Heatmap: Perpetrator Age Group vs Victim Age Group",
      axes = FALSE)
axis(1, at = 1:5, labels = age_order)
axis(2, at = 1:5, labels = age_order, las = 1)

# Add text annotations
for (i in 1:5) {
  for (j in 1:5) {
    text(j, i, age_crosstab[i, j], col = "black", cex = 1.2)
  }
}
dev.off()
cat("Saved: age_crosstab_heatmap.png\n")

# ---- PLOT 8: Victim to Perpetrator Ratio by Age Group (Base R) ----
victim_age_counts <- df[VIC_AGE_GROUP %in% age_order, .N, by = VIC_AGE_GROUP]
perp_age_counts <- df[PERP_AGE_GROUP %in% age_order, .N, by = PERP_AGE_GROUP]

ratio_df <- merge(victim_age_counts, perp_age_counts, 
                  by.x = "VIC_AGE_GROUP", by.y = "PERP_AGE_GROUP", 
                  suffixes = c("_vic", "_perp"))
ratio_df[, Ratio := N_vic / N_perp]
ratio_df[, VIC_AGE_GROUP := factor(VIC_AGE_GROUP, levels = age_order)]
ratio_df <- ratio_df[order(VIC_AGE_GROUP)]

png("victim_perp_ratio_by_age.png", width = 800, height = 500)
par(mar = c(5, 4, 4, 2))
barplot(ratio_df$Ratio, names.arg = ratio_df$VIC_AGE_GROUP,
        col = heat.colors(nrow(ratio_df)),
        main = "Victim-to-Perpetrator Ratio by Age Group",
        xlab = "Age Group", ylab = "Victim-to-Perpetrator Ratio",
        border = "black")
grid()
dev.off()
cat("Saved: victim_perp_ratio_by_age.png\n")

# ---- PLOT 9: Age Proportion Matrix (Base R Heatmap) ----
age_proportion_matrix <- prop.table(age_crosstab, margin = 1)

png("age_proportion_matrix.png", width = 800, height = 600)
par(mar = c(5, 5, 4, 2))
image(1:5, 1:5, age_proportion_matrix, 
      col = colorRampPalette(c("yellow", "orange", "red"))(20),
      xlab = "Victim Age Group", ylab = "Perpetrator Age Group",
      main = "Proportion of Victim Age Groups per Perpetrator Age Group",
      axes = FALSE)
axis(1, at = 1:5, labels = age_order)
axis(2, at = 1:5, labels = age_order, las = 1)

# Add percentage annotations
for (i in 1:5) {
  for (j in 1:5) {
    text(j, i, sprintf("%.1f%%", age_proportion_matrix[i, j] * 100), 
         col = "black", cex = 1)
  }
}
dev.off()
cat("Saved: age_proportion_matrix.png\n")

# ---- PLOT 10: Perpetrator and Victim Race Distributions ----
excluded_races <- c('U', 'UNKNOWN', '(null)')
df_race <- df[!PERP_RACE %in% excluded_races & !VIC_RACE %in% excluded_races]

perp_race_counts <- df_race[, .N, by = PERP_RACE][order(-N)]
vic_race_counts <- df_race[, .N, by = VIC_RACE]

# Align to perpetrator race order
race_order <- perp_race_counts$PERP_RACE
vic_race_counts <- merge(
  data.table(VIC_RACE = race_order),
  vic_race_counts,
  by = "VIC_RACE", all.x = TRUE
)
vic_race_counts[is.na(N), N := 0]

fig10 <- plot_ly() %>%
  add_trace(x = race_order, y = perp_race_counts$N, type = 'bar',
            name = 'Perpetrators', marker = list(color = 'steelblue')) %>%
  add_trace(x = race_order, y = vic_race_counts$N, type = 'bar',
            name = 'Victims', marker = list(color = 'seagreen')) %>%
  layout(
    title = 'Perpetrator and Victim Race Distributions',
    xaxis = list(title = 'Race', tickangle = 45),
    yaxis = list(title = 'Number of Incidents'),
    barmode = 'group',
    template = 'plotly_white'
  )
print(fig10)

# ---- PLOT 11: Race Cross-Tabulation Heatmap (Base R) ----
df_race_clean <- df[!PERP_RACE %in% excluded_races & !VIC_RACE %in% excluded_races]
race_crosstab <- table(df_race_clean$PERP_RACE, df_race_clean$VIC_RACE)

png("race_crosstab_heatmap.png", width = 1000, height = 800)
par(mar = c(6, 8, 4, 2))
image(1:ncol(race_crosstab), 1:nrow(race_crosstab), t(as.matrix(race_crosstab)), 
      col = colorRampPalette(c("white", "yellow", "green", "blue"))(20),
      xlab = "Victim Race", ylab = "Perpetrator Race",
      main = "Heatmap of Shooting Incidents: Perpetrator Race vs. Victim Race",
      axes = FALSE)
axis(1, at = 1:ncol(race_crosstab), labels = colnames(race_crosstab), las = 2)
axis(2, at = 1:nrow(race_crosstab), labels = rownames(race_crosstab), las = 1)

# Add text
for (i in 1:nrow(race_crosstab)) {
  for (j in 1:ncol(race_crosstab)) {
    text(j, i, race_crosstab[i, j], col = "black", cex = 0.8)
  }
}
dev.off()
cat("Saved: race_crosstab_heatmap.png\n")

# ============================================================================
# SECTION 4: SOCIO-ECONOMIC ANALYSIS (if data available)
# ============================================================================

cat("\nAttempting socio-economic analysis...\n")

if (file.exists("NYC_Boroughs_SocioEconomic_2006_2024.csv")) {
  socio_eco_df <- fread("NYC_Boroughs_SocioEconomic_2006_2024.csv")
  
  # Standardize borough names
  df[, BORO := gsub("Staten Island \\(Richmond\\)", "Staten Island", BORO)]
  df[, BORO := gsub("Richmond", "Staten Island", BORO)]
  df[, BORO := tools::toTitleCase(tolower(BORO))]
  
  # Aggregate shooting counts by year and borough
  nypd_counts <- df[, .N, by = .(Year = YEAR, Borough = BORO)]
  
  # Merge datasets
  merged_df <- merge(socio_eco_df, nypd_counts, 
                     by = c("Year", "Borough"), all.x = TRUE)
  merged_df[is.na(N), N := 0]
  setnames(merged_df, "N", "Shooting_Incidents")
  
  # Calculate incidents per 100k
  merged_df[, Incidents_Per_100k := (Shooting_Incidents / Population) * 100000]
  
  # ---- PLOT 12: Correlation Heatmap (Base R) ----
  correlation_cols <- c('Unemployment_Rate', 'Population', 'Median_Income', 
                        'Poverty_Rate', 'Shooting_Incidents')
  cor_matrix <- cor(merged_df[, ..correlation_cols], use = "complete.obs")
  
  png("socioeconomic_correlation_heatmap.png", width = 800, height = 600)
  par(mar = c(5, 5, 4, 2))
  image(1:5, 1:5, cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(20),
        xlab = "", ylab = "",
        main = "Correlation Matrix of Socio-Economic Factors and Shooting Incidents",
        axes = FALSE)
  axis(1, at = 1:5, labels = correlation_cols, las = 2)
  axis(2, at = 1:5, labels = correlation_cols, las = 1)
  
  for (i in 1:5) {
    for (j in 1:5) {
      text(j, i, sprintf("%.2f", cor_matrix[i, j]), col = "black", cex = 1)
    }
  }
  dev.off()
  cat("Saved: socioeconomic_correlation_heatmap.png\n")
  
  # ---- PLOT 13: Time Series for Bronx (Base R) ----
  bronx_df <- merged_df[Borough == "Bronx"][order(Year)]
  
  png("bronx_time_series.png", width = 1200, height = 600)
  par(mar = c(5, 4, 4, 4))
  plot(bronx_df$Year, bronx_df$Shooting_Incidents, type = "o", col = "crimson",
       pch = 19, lwd = 2, xlab = "Year", ylab = "Shooting Incidents",
       main = "Shooting Incidents vs. Unemployment Rate in The Bronx Over Time")
  
  par(new = TRUE)
  plot(bronx_df$Year, bronx_df$Unemployment_Rate, type = "o", col = "steelblue",
       pch = 15, lwd = 2, axes = FALSE, xlab = "", ylab = "")
  axis(4, col = "steelblue", col.axis = "steelblue")
  mtext("Unemployment Rate (%)", side = 4, line = 3, col = "steelblue")
  
  legend("topleft", legend = c("Shooting Incidents", "Unemployment Rate"),
         col = c("crimson", "steelblue"), lty = 1, pch = c(19, 15), lwd = 2)
  grid()
  dev.off()
  cat("Saved: bronx_time_series.png\n")
  
  # ---- PLOT 14: Poverty Rate vs Incidents per 100k (Base R Scatter) ----
  png("poverty_vs_incidents_per_100k.png", width = 1000, height = 700)
  par(mar = c(5, 4, 4, 2))
  plot(merged_df$Poverty_Rate, merged_df$Incidents_Per_100k,
       pch = 19, col = rgb(0, 0, 1, 0.4),
       xlab = "Poverty Rate (%)",
       ylab = "Shooting Incidents per 100,000 Residents",
       main = "Poverty Rate vs. Shooting Incidents per 100k Residents")
  
  # Add regression line
  fit <- lm(Incidents_Per_100k ~ Poverty_Rate, data = merged_df)
  abline(fit, col = "red", lwd = 2)
  grid()
  dev.off()
  cat("Saved: poverty_vs_incidents_per_100k.png\n")
  
  # ---- PLOT 15: Stacked Area Chart - Borough Proportions (Base R) ----
  total_by_year <- merged_df[, .(Total = sum(Shooting_Incidents)), by = Year]
  merged_df <- merge(merged_df, total_by_year, by = "Year")
  merged_df[, Proportion := Shooting_Incidents / Total]
  
  prop_matrix <- dcast(merged_df, Year ~ Borough, value.var = "Proportion", 
                       fun.aggregate = sum)
  prop_matrix[is.na(prop_matrix)] <- 0
  
  png("borough_proportions_stacked_area.png", width = 1200, height = 700)
  par(mar = c(5, 4, 4, 7))
  
  # Create stacked area plot
  years <- prop_matrix$Year
  boroughs <- setdiff(names(prop_matrix), "Year")
  colors_boro <- rainbow(length(boroughs))
  
  plot(years, rep(0, length(years)), type = "n", ylim = c(0, 1),
       xlab = "Year", ylab = "Proportion of Total Incidents",
       main = "Proportion of Total NYC Shooting Incidents by Borough Over Time")
  
  cumsum_vals <- rep(0, length(years))
  for (i in seq_along(boroughs)) {
    boro <- boroughs[i]
    vals <- prop_matrix[[boro]]
    polygon(c(years, rev(years)), 
            c(cumsum_vals, rev(cumsum_vals + vals)),
            col = colors_boro[i], border = NA)
    cumsum_vals <- cumsum_vals + vals
  }
  
  legend("right", legend = boroughs, fill = colors_boro, 
         xpd = TRUE, inset = c(-0.15, 0), cex = 0.8)
  dev.off()
  cat("Saved: borough_proportions_stacked_area.png\n")
  
  # ---- PLOT 16: Per Capita Incidents (Latest Year) ----
  latest_year <- max(merged_df$Year)
  latest_year_df <- merged_df[Year == latest_year][order(-Incidents_Per_100k)]
  
  png("incidents_per_capita_latest_year.png", width = 1000, height = 700)
  par(mar = c(6, 4, 4, 2))
  barplot(latest_year_df$Incidents_Per_100k, 
          names.arg = latest_year_df$Borough,
          col = viridis::viridis(nrow(latest_year_df)),
          main = sprintf("Shooting Incidents Per 100,000 Residents by Borough (%d)", 
                         latest_year),
          xlab = "Borough", ylab = "Shooting Incidents per 100,000 Residents",
          las = 2)
  grid()
  dev.off()
  cat("Saved: incidents_per_capita_latest_year.png\n")
  
} else {
  cat("Socio-economic data file not found. Skipping related plots.\n")
}

# ============================================================================
# SECTION 5: LOCATION-BASED PLOTS
# ============================================================================

cat("\nGenerating location-based plots...\n")

# ---- PLOT 17: Murder Flag Distribution (Base R Pie) ----
murder_counts <- table(df$STATISTICAL_MURDER_FLAG)
murder_labels <- c("Not Murder", "Murder")[as.integer(names(murder_counts)) + 1]

png("murder_flag_distribution.png", width = 700, height = 700)
pie(murder_counts, labels = murder_labels, 
    col = c("blue", "red"),
    main = "Murder Distribution")
percentages <- round(murder_counts / sum(murder_counts) * 100, 1)
legend("topright", legend = paste(murder_labels, percentages, "%"), 
       fill = c("blue", "red"))
dev.off()
cat("Saved: murder_flag_distribution.png\n")

# ---- PLOT 18: Location Classification (Base R Bar) ----
df_loc_class <- df[LOC_CLASSFCTN_DESC != "UNKNOWN"]
loc_class_counts <- df_loc_class[, .N, by = LOC_CLASSFCTN_DESC][order(-N)]

if (nrow(loc_class_counts) > 0) {
  png("location_classification_bar.png", width = 1000, height = 600)
  par(mar = c(8, 4, 4, 2))
  barplot(loc_class_counts$N, 
          names.arg = loc_class_counts$LOC_CLASSFCTN_DESC,
          col = rainbow(nrow(loc_class_counts)),
          main = "Location Classification Description",
          xlab = "", ylab = "Incident Count",
          las = 2)
  dev.off()
  cat("Saved: location_classification_bar.png\n")
}

# ---- PLOT 19: Location of Occurrence (Base R Pie) ----
loc_occur_counts <- table(df$LOC_OF_OCCUR_DESC)

png("location_occurrence_pie.png", width = 800, height = 800)
pie(loc_occur_counts, 
    main = "Location of Occurrence",
    col = rainbow(length(loc_occur_counts)))
dev.off()
cat("Saved: location_occurrence_pie.png\n")

# ---- PLOT 20: Borough Bar Chart (Base R) ----
boro_counts <- df[, .N, by = BORO][order(-N)]

png("borough_distribution_bar.png", width = 1000, height = 600)
par(mar = c(5, 4, 4, 2))
barplot(boro_counts$N, 
        names.arg = boro_counts$BORO,
        col = rainbow(nrow(boro_counts)),
        main = "Distribution of Shooting Incidents by Borough",
        xlab = "Borough", ylab = "Incident Count")
dev.off()
cat("Saved: borough_distribution_bar.png\n")

# ---- PLOT 21: Detailed Location Description (Base R Bar) ----
df_loc_desc <- df[LOCATION_DESC != "UNKNOWN"]
loc_desc_counts <- df_loc_desc[, .N, by = LOCATION_DESC][order(-N)][1:min(20, .N)]

png("location_description_bar.png", width = 1200, height = 600)
par(mar = c(10, 4, 4, 2))
barplot(loc_desc_counts$N, 
        names.arg = loc_desc_counts$LOCATION_DESC,
        col = rainbow(nrow(loc_desc_counts)),
        main = "Detailed Location Description (Top 20)",
        xlab = "", ylab = "Incident Count",
        las = 2, cex.names = 0.7)
dev.off()
cat("Saved: location_description_bar.png\n")

# ============================================================================
# SECTION 6: TIME SERIES ANALYSIS
# ============================================================================

cat("\nGenerating time series analysis plots...\n")

# Prepare monthly time series
df2 <- copy(df)
setorder(df2, OCCUR_DATETIME)
df2[, YearMonth := format(OCCUR_DATETIME, "%Y-%m")]
monthly_ts <- df2[, .N, by = YearMonth][order(YearMonth)]
monthly_ts[, Date := as.Date(paste0(YearMonth, "-01"))]

# ---- PLOT 22: Monthly Incident Counts (Base R) ----
png("monthly_incident_counts.png", width = 1200, height = 400)
par(mar = c(5, 4, 4, 2))
plot(monthly_ts$Date, monthly_ts$N, type = "o", pch = 19,
     col = "darkblue", lwd = 1,
     xlab = "Month", ylab = "Count",
     main = "Monthly Incident Counts")
grid(col = "gray", lty = "dotted")
dev.off()
cat("Saved: monthly_incident_counts.png\n")

# ---- PLOT 23: Time Series Decomposition ----
# Create a proper time series object
monthly_values <- monthly_ts$N
ts_data <- ts(monthly_values, frequency = 12, 
              start = c(min_year, 1))

if (length(monthly_values) >= 24) {
  png("time_series_decomposition.png", width = 1200, height = 800)
  decomp <- decompose(ts_data, type = "additive")
  plot(decomp)
  dev.off()
  cat("Saved: time_series_decomposition.png\n")
}

# ---- PLOT 24: Rolling Statistics (Base R) ----
monthly_ts[, Rolling_Mean := rollmean(N, k = 12, fill = NA, align = "center")]
monthly_ts[, Rolling_SD := rollapply(N, width = 12, FUN = sd, 
                                     fill = NA, align = "center")]

png("rolling_statistics.png", width = 1200, height = 400)
par(mar = c(5, 4, 4, 2))
plot(monthly_ts$Date, monthly_ts$N, type = "l", col = "black",
     xlab = "Date", ylab = "Value",
     main = "Rolling Mean and Standard Deviation",
     ylim = range(c(monthly_ts$N, monthly_ts$Rolling_Mean), na.rm = TRUE))
lines(monthly_ts$Date, monthly_ts$Rolling_Mean, col = "blue", lwd = 2)
lines(monthly_ts$Date, monthly_ts$Rolling_SD, col = "red", lwd = 2)
legend("topright", legend = c("Monthly", "Rolling Mean (12)", "Rolling SD (12)"),
       col = c("black", "blue", "red"), lty = 1, lwd = c(1, 2, 2))
dev.off()
cat("Saved: rolling_statistics.png\n")

# ============================================================================
# SECTION 7: MORE EDA PLOTS
# ============================================================================

cat("\nGenerating additional EDA plots...\n")

# ---- PLOT 25: Perpetrator Age Group Distribution (Base R) ----
perp_age_dist <- df[PERP_AGE_GROUP %in% age_order, .N, by = PERP_AGE_GROUP]
perp_age_dist <- merge(data.table(PERP_AGE_GROUP = age_order), perp_age_dist, 
                       by = "PERP_AGE_GROUP", all.x = TRUE)
perp_age_dist[is.na(N), N := 0]

png("perpetrator_age_distribution.png", width = 1200, height = 600)
par(mar = c(5, 7, 4, 2))
barplot(perp_age_dist$N, names.arg = perp_age_dist$PERP_AGE_GROUP,
        horiz = TRUE, col = "darkviolet",
        main = "Distribution of Perpetrator Age Groups",
        xlab = "Number of Incidents", ylab = "")
text(perp_age_dist$N, 1:nrow(perp_age_dist) * 1.2 - 0.5, 
     labels = format(perp_age_dist$N, big.mark = ","),
     pos = 4, cex = 0.9)
grid()
dev.off()
cat("Saved: perpetrator_age_distribution.png\n")

# ---- PLOT 26: Victim Age Group Distribution (Base R) ----
vic_age_dist <- df[VIC_AGE_GROUP %in% age_order, .N, by = VIC_AGE_GROUP]
vic_age_dist <- merge(data.table(VIC_AGE_GROUP = age_order), vic_age_dist, 
                      by = "VIC_AGE_GROUP", all.x = TRUE)
vic_age_dist[is.na(N), N := 0]

png("victim_age_distribution.png", width = 1200, height = 600)
par(mar = c(5, 7, 4, 2))
barplot(vic_age_dist$N, names.arg = vic_age_dist$VIC_AGE_GROUP,
        horiz = TRUE, col = "darkviolet",
        main = "Distribution of Victim Age Groups",
        xlab = "Number of Incidents", ylab = "")
text(vic_age_dist$N, 1:nrow(vic_age_dist) * 1.2 - 0.5, 
     labels = format(vic_age_dist$N, big.mark = ","),
     pos = 4, cex = 0.9)
grid()
dev.off()
cat("Saved: victim_age_distribution.png\n")

# ---- PLOT 27: Victim Race Distribution (Base R) ----
vic_race_dist <- df[!VIC_RACE %in% excluded_races, .N, by = VIC_RACE][order(N)]

png("victim_race_distribution.png", width = 2000, height = 800)
par(mar = c(5, 10, 4, 2))
barplot(vic_race_dist$N, names.arg = vic_race_dist$VIC_RACE,
        horiz = TRUE, col = "darkred",
        main = "Distribution of Victim Race",
        xlab = "Number of Incidents", ylab = "",
        las = 1)
text(vic_race_dist$N, 1:nrow(vic_race_dist) * 1.2 - 0.5, 
     labels = format(vic_race_dist$N, big.mark = ","),
     pos = 4, cex = 0.9)
grid()
dev.off()
cat("Saved: victim_race_distribution.png\n")

# ---- PLOT 28: Victim Sex Distribution (Base R Pie) ----
vic_sex_dist <- table(df[VIC_SEX %in% c('M', 'F')]$VIC_SEX)
sex_labels <- c("Male (M)", "Female (F)")[match(names(vic_sex_dist), c("M", "F"))]
sex_labels_full <- paste(sex_labels, format(vic_sex_dist, big.mark = ","))

png("victim_sex_distribution.png", width = 800, height = 800)
pie(vic_sex_dist, labels = "", 
    col = c("lightblue", "pink"),
    main = "Distribution of Victim Sex")
percentages <- round(vic_sex_dist / sum(vic_sex_dist) * 100, 1)
legend_text <- paste(sex_labels_full, sprintf("(%.1f%%)", percentages))
legend("topright", legend = legend_text, 
       fill = c("lightblue", "pink"),
       title = "Sex (Count)")
dev.off()
cat("Saved: victim_sex_distribution.png\n")

# ============================================================================
# SECTION 8: SPECIAL EVENTS/HOLIDAYS ANALYSIS
# ============================================================================

cat("\nGenerating special events/holidays analysis...\n")

# Define helper functions for special dates
get_nth_weekday <- function(year, month, weekday, n) {
  first_day <- as.Date(paste(year, month, 1, sep = "-"))
  first_weekday <- as.numeric(format(first_day, "%u"))  # 1=Mon, 7=Sun
  days_to_first <- (weekday - first_weekday + 7) %% 7
  target_date <- first_day + days_to_first + (n - 1) * 7
  if (as.numeric(format(target_date, "%m")) == month) {
    return(target_date)
  }
  return(NA)
}

get_last_weekday <- function(year, month, weekday) {
  if (month == 12) {
    last_day <- as.Date(paste(year + 1, 1, 1, sep = "-")) - 1
  } else {
    last_day <- as.Date(paste(year, month + 1, 1, sep = "-")) - 1
  }
  last_weekday_num <- as.numeric(format(last_day, "%u"))
  days_back <- (last_weekday_num - weekday + 7) %% 7
  return(last_day - days_back)
}

# Pre-calculate special dates (2006-2024)
special_dates <- list()
years_range <- 2006:2024

for (year in years_range) {
  # Fixed holidays
  special_dates[["New Year's Day"]] <- c(special_dates[["New Year's Day"]], 
                                         as.Date(paste(year, "01", "01", sep = "-")))
  special_dates[["St. Patrick's Day"]] <- c(special_dates[["St. Patrick's Day"]], 
                                            as.Date(paste(year, "03", "17", sep = "-")))
  special_dates[["Juneteenth"]] <- c(special_dates[["Juneteenth"]], 
                                     as.Date(paste(year, "06", "19", sep = "-")))
  special_dates[["Independence Day"]] <- c(special_dates[["Independence Day"]], 
                                           as.Date(paste(year, "07", "04", sep = "-")))
  special_dates[["9/11 Remembrance"]] <- c(special_dates[["9/11 Remembrance"]], 
                                           as.Date(paste(year, "09", "11", sep = "-")))
  special_dates[["Halloween"]] <- c(special_dates[["Halloween"]], 
                                    as.Date(paste(year, "10", "31", sep = "-")))
  special_dates[["Veterans Day"]] <- c(special_dates[["Veterans Day"]], 
                                       as.Date(paste(year, "11", "11", sep = "-")))
  special_dates[["Christmas"]] <- c(special_dates[["Christmas"]], 
                                    as.Date(paste(year, "12", "24", sep = "-")),
                                    as.Date(paste(year, "12", "25", sep = "-")))
  special_dates[["New Year's Eve"]] <- c(special_dates[["New Year's Eve"]], 
                                         as.Date(paste(year, "12", "31", sep = "-")))
  
  # Variable holidays (weekday-based)
  special_dates[["MLK Jr. Day"]] <- c(special_dates[["MLK Jr. Day"]], 
                                      get_nth_weekday(year, 1, 1, 3))  # 3rd Mon in Jan
  special_dates[["Presidents' Day"]] <- c(special_dates[["Presidents' Day"]], 
                                          get_nth_weekday(year, 2, 1, 3))  # 3rd Mon in Feb
  special_dates[["Memorial Day"]] <- c(special_dates[["Memorial Day"]], 
                                       get_last_weekday(year, 5, 1))  # Last Mon in May
  special_dates[["Labor Day"]] <- c(special_dates[["Labor Day"]], 
                                    get_nth_weekday(year, 9, 1, 1))  # 1st Mon in Sep
  special_dates[["Columbus Day"]] <- c(special_dates[["Columbus Day"]], 
                                       get_nth_weekday(year, 10, 1, 2))  # 2nd Mon in Oct
  special_dates[["Thanksgiving"]] <- c(special_dates[["Thanksgiving"]], 
                                       get_nth_weekday(year, 11, 4, 4))  # 4th Thu in Nov
}

# Remove NAs
special_dates <- lapply(special_dates, function(x) x[!is.na(x)])

# Classify dates
classify_period <- function(date) {
  for (period_name in names(special_dates)) {
    if (date %in% special_dates[[period_name]]) {
      return(period_name)
    }
  }
  return("Normal Day")
}

df[, PERIOD_TYPE := sapply(DATE, classify_period)]

# Count incidents by period
period_counts <- df[PERIOD_TYPE != "Normal Day", .N, by = PERIOD_TYPE][order(N)]

# ---- PLOT 29: Incidents by Special Period (Base R) ----
png("incidents_by_special_period.png", width = 1600, height = 700)
par(mar = c(5, 15, 4, 2))
barplot(period_counts$N, names.arg = period_counts$PERIOD_TYPE,
        horiz = TRUE, col = "darkblue",
        main = "Incident Count by Special Event/Holiday (2006-2024)",
        xlab = "Total Number of Incidents", ylab = "",
        las = 1)
text(period_counts$N, 1:nrow(period_counts) * 1.2 - 0.5, 
     labels = format(period_counts$N, big.mark = ","),
     pos = 4, cex = 0.8)
grid()
dev.off()
cat("Saved: incidents_by_special_period.png\n")

# ============================================================================
# SECTION 9: WEEKLY CONTINUOUS ANALYSIS
# ============================================================================

cat("\nGenerating weekly continuous analysis...\n")

# Aggregate by week
df_weekly <- copy(df)
df_weekly[, Week := format(OCCUR_DATETIME, "%Y-%U")]
weekly_counts <- df_weekly[, .N, by = Week][order(Week)]
weekly_counts[, Date := as.Date(paste0(Week, "-1"), format = "%Y-%U-%u")]

# ---- PLOT 30: Weekly Shooting Counts (Base R) ----
png("weekly_shooting_counts.png", width = 1600, height = 600)
par(mar = c(5, 4, 4, 2))
plot(weekly_counts$Date, weekly_counts$N, type = "l", col = "darkred", lwd = 1.5,
     xlab = "Date", ylab = "Number of Shooting Incidents",
     main = "Weekly Shooting Incident Counts (2006-2024)")
grid(col = "gray", lty = "dotted")
dev.off()
cat("Saved: weekly_shooting_counts.png\n")

# ============================================================================
# SECTION 10: SEASONAL AND DAY TYPE ANALYSIS
# ============================================================================

cat("\nGenerating seasonal and day type analysis...\n")

# Define season function
get_season <- function(month) {
  ifelse(month %in% c(12, 1, 2), "Winter",
         ifelse(month %in% c(3, 4, 5), "Spring",
                ifelse(month %in% c(6, 7, 8), "Summer", "Fall")))
}

df[, SEASON := get_season(as.integer(format(OCCUR_DATETIME, "%m")))]

# Add day of year
df[, DAY_OF_YEAR := as.integer(format(OCCUR_DATETIME, "%j"))]

# Simple day type classification
df[, DAY_TYPE := ifelse(PERIOD_TYPE == "Normal Day", "Normal Day", "Special Day")]

# ---- PLOT 31: Normal Day Comprehensive Analysis ----
normal_days <- df[DAY_TYPE == "Normal Day"]
normal_monthly <- normal_days[, .N, by = .(MONTH = as.integer(format(OCCUR_DATETIME, "%m")))]
normal_monthly <- merge(data.table(MONTH = 1:12), normal_monthly, by = "MONTH", all.x = TRUE)
normal_monthly[is.na(N), N := 0]

month_names <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

png("normal_day_monthly_distribution.png", width = 1400, height = 700)
par(mar = c(6, 4, 4, 2))
bars <- barplot(normal_monthly$N, names.arg = month_names,
                col = "steelblue", border = "black",
                main = "Normal Days: Monthly Distribution\n(Excluding Holidays & Summer)",
                xlab = "", ylab = "Number of Incidents",
                las = 2)
text(bars, normal_monthly$N, labels = format(normal_monthly$N, big.mark = ","),
     pos = 3, cex = 0.8)
grid(ny = NULL, nx = NA)
dev.off()
cat("Saved: normal_day_monthly_distribution.png\n")

# ---- PLOT 32: Seasonal Analysis ----
seasonal_counts <- df[, .N, by = SEASON]
season_order <- c("Winter", "Spring", "Summer", "Fall")
seasonal_counts <- merge(data.table(SEASON = season_order), seasonal_counts, 
                         by = "SEASON", all.x = TRUE)
seasonal_counts[is.na(N), N := 0]

season_colors <- c("#4682B4", "#90EE90", "#FF6B6B", "#FFA500")

png("seasonal_distribution.png", width = 1000, height = 700)
par(mar = c(5, 4, 4, 2))
bars <- barplot(seasonal_counts$N, names.arg = seasonal_counts$SEASON,
                col = season_colors, border = "black",
                main = "Total Incidents by Season (2006-2024)",
                xlab = "Season", ylab = "Total Incidents")
text(bars, seasonal_counts$N, 
     labels = paste0(format(seasonal_counts$N, big.mark = ","), "\n",
                     sprintf("(%.1f%%)", seasonal_counts$N / sum(seasonal_counts$N) * 100)),
     pos = 3, cex = 0.9, font = 2)
grid(ny = NULL, nx = NA)
dev.off()
cat("Saved: seasonal_distribution.png\n")

# ---- PLOT 33: Seasonal Trends Over Time ----
seasonal_yearly <- df[, .N, by = .(YEAR, SEASON)]
seasonal_yearly <- dcast(seasonal_yearly, YEAR ~ SEASON, value.var = "N", fill = 0)

png("seasonal_trends_over_time.png", width = 1400, height = 700)
par(mar = c(5, 4, 4, 7), xpd = FALSE)
plot(seasonal_yearly$YEAR, seasonal_yearly$Winter, type = "o", 
     col = season_colors[1], lwd = 2.5, pch = 19,
     ylim = range(c(seasonal_yearly$Winter, seasonal_yearly$Spring, 
                    seasonal_yearly$Summer, seasonal_yearly$Fall)),
     xlab = "Year", ylab = "Number of Incidents",
     main = "Seasonal Trends Over Time (2006-2024)")
lines(seasonal_yearly$YEAR, seasonal_yearly$Spring, type = "o", 
      col = season_colors[2], lwd = 2.5, pch = 19)
lines(seasonal_yearly$YEAR, seasonal_yearly$Summer, type = "o", 
      col = season_colors[3], lwd = 2.5, pch = 19)
lines(seasonal_yearly$YEAR, seasonal_yearly$Fall, type = "o", 
      col = season_colors[4], lwd = 2.5, pch = 19)

# Add 2020 annotation
abline(v = 2020, col = "red", lty = 2, lwd = 2)
text(2020, max(seasonal_yearly[, -1]) * 0.95, "2020\nSurge", 
     col = "red", font = 2, cex = 0.9)

par(xpd = TRUE)
legend("topright", legend = season_order, col = season_colors, 
       lty = 1, lwd = 2.5, pch = 19, inset = c(-0.15, 0), cex = 0.9)
grid()
dev.off()
cat("Saved: seasonal_trends_over_time.png\n")

# ============================================================================
# SECTION 11: GENDER ANALYSIS
# ============================================================================

cat("\nGenerating gender analysis plots...\n")

# Clean sex variables
df[, VIC_SEX_CLEAN := ifelse(VIC_SEX %in% c('M', 'F'), VIC_SEX, NA_character_)]
df[, PERP_SEX_CLEAN := ifelse(PERP_SEX %in% c('M', 'F'), PERP_SEX, NA_character_)]

df_clean <- df[!is.na(VIC_SEX_CLEAN) & !is.na(PERP_SEX_CLEAN)]
df_clean[, GENDER_COMBO := paste(PERP_SEX_CLEAN, "→", VIC_SEX_CLEAN)]

cat(sprintf("\n📊 Clean M/F records: %s (%.1f%%)\n", 
            format(nrow(df_clean), big.mark = ","), 
            nrow(df_clean)/nrow(df)*100))

# ---- PLOT 34: Gender Matrix (Absolute Counts) ----
gender_matrix <- table(df_clean$VIC_SEX_CLEAN, df_clean$PERP_SEX_CLEAN)

png("gender_matrix_counts.png", width = 800, height = 600)
par(mar = c(5, 5, 4, 2))
image(1:2, 1:2, t(gender_matrix), 
      col = colorRampPalette(c("yellow", "orange", "red"))(20),
      xlab = "Perpetrator Gender", ylab = "Victim Gender",
      main = "Victim-Perpetrator Gender Matrix\n(Absolute Counts + % of Total)",
      axes = FALSE)
axis(1, at = 1:2, labels = c("F", "M"))
axis(2, at = 1:2, labels = c("F", "M"), las = 1)

# Add annotations
for (i in 1:2) {
  for (j in 1:2) {
    count <- gender_matrix[i, j]
    pct <- count / nrow(df_clean) * 100
    text(j, i, sprintf("%d\n(%.1f%%)", count, pct), 
         col = "black", cex = 1.2, font = 2)
  }
}
dev.off()
cat("Saved: gender_matrix_counts.png\n")

# ---- PLOT 35: Gender Matrix (Percentages) ----
gender_matrix_pct <- prop.table(gender_matrix) * 100

png("gender_matrix_percent.png", width = 800, height = 600)
par(mar = c(5, 5, 4, 2))
image(1:2, 1:2, t(gender_matrix_pct), 
      col = colorRampPalette(c("green", "yellow", "red"))(20),
      xlab = "Perpetrator Gender", ylab = "Victim Gender",
      main = "Gender Interaction Rates\n(% of All Incidents)",
      axes = FALSE)
axis(1, at = 1:2, labels = c("F", "M"))
axis(2, at = 1:2, labels = c("F", "M"), las = 1)

for (i in 1:2) {
  for (j in 1:2) {
    text(j, i, sprintf("%.1f%%", gender_matrix_pct[i, j]), 
         col = "black", cex = 1.4, font = 2)
  }
}
dev.off()
cat("Saved: gender_matrix_percent.png\n")

# ---- PLOT 36: Lethality by Gender Combination ----
df_clean[, STATISTICAL_MURDER_FLAG := as.logical(
  ifelse(is.na(STATISTICAL_MURDER_FLAG), FALSE, STATISTICAL_MURDER_FLAG)
)]

murder_by_combo <- df_clean[, .(
  Total = .N,
  Murders = sum(STATISTICAL_MURDER_FLAG),
  Murder_Rate = mean(STATISTICAL_MURDER_FLAG) * 100
), by = GENDER_COMBO][order(-Total)]

png("lethality_by_gender_combo.png", width = 1000, height = 600)
par(mar = c(7, 4, 4, 2))
bars <- barplot(murder_by_combo$Murder_Rate, 
                names.arg = murder_by_combo$GENDER_COMBO,
                col = "coral", border = "black",
                main = "Lethality Rate by Gender Combination\n(% Resulting in Murder)",
                xlab = "", ylab = "Murder Rate (%)",
                las = 2)
text(bars, murder_by_combo$Murder_Rate, 
     labels = sprintf("%.1f%%", murder_by_combo$Murder_Rate),
     pos = 3, cex = 0.9)
grid(ny = NULL, nx = NA)
dev.off()
cat("Saved: lethality_by_gender_combo.png\n")

# ---- PLOT 37: Victim Age by Gender Combination ----
df_clean[, VIC_AGE_GROUP_CLEAN := ifelse(
  VIC_AGE_GROUP %in% age_order, VIC_AGE_GROUP, NA_character_
)]

df_age <- df_clean[!is.na(VIC_AGE_GROUP_CLEAN)]
age_gender <- df_age[, .N, by = .(VIC_AGE_GROUP_CLEAN, GENDER_COMBO)]

# Get combo order
combo_order <- murder_by_combo$GENDER_COMBO

# Create matrix for grouped bar plot
age_matrix_list <- list()
for (combo in combo_order) {
  combo_data <- age_gender[GENDER_COMBO == combo]
  combo_vec <- rep(0, length(age_order))
  names(combo_vec) <- age_order
  for (i in 1:nrow(combo_data)) {
    age <- combo_data$VIC_AGE_GROUP_CLEAN[i]
    if (age %in% age_order) {
      combo_vec[age] <- combo_data$N[i]
    }
  }
  age_matrix_list[[combo]] <- combo_vec
}

age_matrix_plot <- do.call(cbind, age_matrix_list)

png("age_by_gender_combo.png", width = 1200, height = 700)
par(mar = c(7, 4, 4, 7), xpd = FALSE)
barplot(age_matrix_plot, beside = TRUE, 
        col = rainbow(length(age_order)),
        border = "black",
        main = "Victim Age Distribution by Gender Combination",
        xlab = "", ylab = "Number of Victims",
        las = 2)
par(xpd = TRUE)
legend("topright", legend = age_order, fill = rainbow(length(age_order)),
       title = "Victim Age Group", inset = c(-0.12, 0), cex = 0.8)
grid(ny = NULL, nx = NA)
dev.off()
cat("Saved: age_by_gender_combo.png\n")

# ---- PLOT 38: Summary Statistics Table ----
summary_data <- data.table()
for (combo in combo_order) {
  combo_df <- df_clean[GENDER_COMBO == combo]
  summary_data <- rbind(summary_data, data.table(
    Gender_Combo = combo,
    Total_Incidents = format(nrow(combo_df), big.mark = ","),
    Pct_of_Total = sprintf("%.1f%%", nrow(combo_df) / nrow(df_clean) * 100),
    Total_Murders = format(sum(combo_df$STATISTICAL_MURDER_FLAG), big.mark = ","),
    Murder_Rate = sprintf("%.1f%%", mean(combo_df$STATISTICAL_MURDER_FLAG) * 100)
  ))
}

png("gender_summary_table.png", width = 1000, height = 600)
par(mar = c(1, 1, 3, 1))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(0, 1))

# Title
text(0.5, 0.95, "Summary Statistics by Gender Combination", 
     cex = 1.5, font = 2)

# Create table layout
n_rows <- nrow(summary_data) + 1
n_cols <- ncol(summary_data)
cell_height <- 0.8 / n_rows
cell_width <- 0.9 / n_cols

# Header row
for (j in 1:n_cols) {
  rect((j-1) * cell_width + 0.05, 0.85 - cell_height, 
       j * cell_width + 0.05, 0.85,
       col = "steelblue", border = "black")
  text((j-0.5) * cell_width + 0.05, 0.85 - cell_height/2, 
       names(summary_data)[j], col = "white", font = 2, cex = 0.9)
}

# Data rows
for (i in 1:nrow(summary_data)) {
  y_top <- 0.85 - i * cell_height
  y_bottom <- 0.85 - (i+1) * cell_height
  
  # Alternate row colors
  row_col <- if (i %% 2 == 0) "gray95" else "white"
  
  for (j in 1:n_cols) {
    rect((j-1) * cell_width + 0.05, y_bottom, 
         j * cell_width + 0.05, y_top,
         col = row_col, border = "black")
    
    cell_text <- as.character(summary_data[i, ..j][[1]])
    text_font <- if (j == 1) 2 else 1  # Bold first column
    text((j-0.5) * cell_width + 0.05, (y_top + y_bottom)/2, 
         cell_text, cex = 0.85, font = text_font)
  }
}
dev.off()
cat("Saved: gender_summary_table.png\n")

# Statistical summary to console
cat("\n" , paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("📊 STATISTICAL SUMMARY\n")
cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")

for (i in 1:nrow(murder_by_combo)) {
  combo <- murder_by_combo$GENDER_COMBO[i]
  combo_df <- df_clean[GENDER_COMBO == combo]
  murders <- sum(combo_df$STATISTICAL_MURDER_FLAG)
  total <- nrow(combo_df)
  rate <- murders / total * 100
  
  cat(sprintf("  • %s: %s incidents | %s murders | %.1f%% lethality\n",
              format(combo, width = 12),
              format(total, big.mark = ",", width = 6),
              format(murders, big.mark = ",", width = 4),
              rate))
}

# Relative risk
if ("M → M" %in% combo_order && "M → F" %in% combo_order) {
  mm_rate <- mean(df_clean[GENDER_COMBO == "M → M"]$STATISTICAL_MURDER_FLAG)
  mf_rate <- mean(df_clean[GENDER_COMBO == "M → F"]$STATISTICAL_MURDER_FLAG)
  
  if (mf_rate > 0) {
    rr <- mm_rate / mf_rate
    cat(sprintf("\n🔍 Relative Risk (M→M vs M→F): %.2fx\n", rr))
    if (rr > 1) {
      cat(sprintf("   → Male-on-Male shootings are %.2fx more likely to be fatal.\n", rr))
    } else {
      cat(sprintf("   → Male-on-Female shootings are %.2fx more likely to be fatal.\n", 1/rr))
    }
  }
}

# ============================================================================
# SECTION 12: CUMULATIVE PLOTS
# ============================================================================

cat("\nGenerating cumulative analysis plots...\n")

df_cum <- copy(df)
setorder(df_cum, OCCUR_DATETIME)
df_cum[, CUMULATIVE_COUNT := 1:.N]
df_cum[, DAYS_SINCE_START := as.numeric(OCCUR_DATETIME - min(OCCUR_DATETIME), units = "days")]

# ---- PLOT 39: Full Timeline with Regime Trends ----
regime_dates <- list(
  'Pre-2020' = c('2006-01-01', '2019-12-31'),
  '2020-2021 Surge' = c('2020-01-01', '2021-12-31'),
  'Post-2022' = c('2022-01-01', '2024-12-31')
)
regime_colors_vec <- c('Pre-2020' = 'green', '2020-2021 Surge' = 'red', 
                       'Post-2022' = 'orange')

png("cumulative_full_timeline.png", width = 1400, height = 700)
par(mar = c(5, 4, 4, 2))
plot(df_cum$OCCUR_DATETIME, df_cum$CUMULATIVE_COUNT, type = "l",
     col = "darkblue", lwd = 2.5,
     xlab = "Date", ylab = "Cumulative Shootings",
     main = "Cumulative NYPD Shootings (2006–2024) with Regime-Specific Trends")

# Add regime trend lines
for (label in names(regime_dates)) {
  dates <- regime_dates[[label]]
  start_date <- as.POSIXct(dates[1])
  end_date <- as.POSIXct(dates[2])
  
  regime_df <- df_cum[OCCUR_DATETIME >= start_date & OCCUR_DATETIME <= end_date]
  
  if (nrow(regime_df) > 1) {
    x <- as.numeric(regime_df$OCCUR_DATETIME - min(regime_df$OCCUR_DATETIME), units = "days")
    y <- regime_df$CUMULATIVE_COUNT - regime_df$CUMULATIVE_COUNT[1]
    fit <- lm(y ~ x)
    slope <- coef(fit)[2]
    
    trend_y <- slope * x + regime_df$CUMULATIVE_COUNT[1]
    lines(regime_df$OCCUR_DATETIME, trend_y, col = regime_colors_vec[label],
          lwd = 2, lty = 2)
  }
}

# Add milestones
milestones <- list(
  c("2020-03-15", "COVID-19\nLockdown"),
  c("2020-05-25", "George Floyd")
)

for (milestone in milestones) {
  date <- as.POSIXct(milestone[1])
  label_text <- milestone[2]
  if (any(df_cum$OCCUR_DATETIME >= date)) {
    cum_val <- df_cum[OCCUR_DATETIME >= date][1]$CUMULATIVE_COUNT
    abline(v = date, col = "red", lty = 3, lwd = 2)
    text(date, cum_val, label_text, srt = 90, adj = c(-0.1, 0.5), 
         cex = 0.8, font = 2)
  }
}

legend("topleft", 
       legend = c("Cumulative", "Pre-2020", "2020-2021 Surge", "Post-2022"),
       col = c("darkblue", "green", "red", "orange"),
       lty = c(1, 2, 2, 2), lwd = 2, cex = 0.9)
grid()
dev.off()
cat("Saved: cumulative_full_timeline.png\n")

# ---- PLOT 40: Cumulative by Borough ----
png("cumulative_by_borough.png", width = 1400, height = 700)
par(mar = c(5, 4, 4, 7), xpd = FALSE)
plot(1, type = "n", xlim = range(df_cum$OCCUR_DATETIME), 
     ylim = c(0, max(df_cum$CUMULATIVE_COUNT)),
     xlab = "Date", ylab = "Cumulative Shootings",
     main = "Cumulative Shootings by Borough (2006–2024)")

for (boro in unique(df$BORO)[!is.na(unique(df$BORO))]) {
  boro_df <- df_cum[BORO == boro]
  setorder(boro_df, OCCUR_DATETIME)
  boro_df[, CUM := 1:.N]
  
  boro_col <- borough_colors[boro]
  if (is.na(boro_col)) boro_col <- "gray"
  
  lines(boro_df$OCCUR_DATETIME, boro_df$CUM, col = boro_col, lwd = 2.5)
}

par(xpd = TRUE)
legend("topright", legend = names(borough_colors), 
       col = unlist(borough_colors), lty = 1, lwd = 2.5,
       inset = c(-0.15, 0), cex = 0.8)
grid()
dev.off()
cat("Saved: cumulative_by_borough.png\n")

# ---- PLOT 41: Cumulative by Season ----
season_colors_cum <- c('Winter' = '#4682B4', 'Spring' = '#90EE90',
                       'Summer' = '#FF6B6B', 'Fall' = '#FFA500')

png("cumulative_by_season.png", width = 1400, height = 700)
par(mar = c(5, 4, 4, 7), xpd = FALSE)
plot(1, type = "n", xlim = range(df_cum$OCCUR_DATETIME), 
     ylim = c(0, max(df_cum$CUMULATIVE_COUNT)),
     xlab = "Date", ylab = "Cumulative Shootings",
     main = "Cumulative Shootings by Season (2006–2024)")

for (season in c('Winter', 'Spring', 'Summer', 'Fall')) {
  season_df <- df_cum[SEASON == season]
  setorder(season_df, OCCUR_DATETIME)
  season_df[, CUM := 1:.N]
  
  lines(season_df$OCCUR_DATETIME, season_df$CUM, 
        col = season_colors_cum[season], lwd = 2.5)
}

par(xpd = TRUE)
legend("topright", legend = names(season_colors_cum), 
       col = unlist(season_colors_cum), lty = 1, lwd = 2.5,
       inset = c(-0.15, 0), cex = 0.8)
grid()
dev.off()
cat("Saved: cumulative_by_season.png\n")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n" , paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("✅ ANALYSIS COMPLETE!\n")
cat(paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("\nGenerated plots:\n")
cat("  • Interactive plots (plotly): 10 plots\n")
cat("  • Static plots (base R): 31+ plots\n")
cat("  • Total: 41+ comprehensive visualizations\n")
cat("\nAll plots have been saved as PNG files in the working directory.\n")
cat("Interactive plotly figures are displayed in your viewer/browser.\n")
cat("\n" , paste(rep("=", 70), collapse = ""), "\n", sep = "")