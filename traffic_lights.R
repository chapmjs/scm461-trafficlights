# ============================================================================
# Week 2 Traffic Light Report - The Goal T/I/OE Analysis
# Student: John Chapman
# Course: SCM Operations Management
# Assignment: Diagnose plant health using Throughput, Inventory, and OE
# ============================================================================

# NOTE TO STUDENTS: Before running this script, you MUST update the file path
# below to match where you saved the CSV file on your computer.
#
# Windows example: "C:/Users/YourName/Downloads/goal_data_2years-1.csv"
# Mac example: "/Users/YourName/Downloads/goal_data_2years-1.csv"

# ============================================================================
# STEP 1: IMPORT THE CSV FILE
# ============================================================================

# Replace this path with YOUR full file path to the CSV
# goal_data <- read.csv("/mnt/user-data/uploads/goal_data_2years.csv")
goal_data <- read.csv("goal_data_2years.csv")

# Quick check to see if data loaded correctly
cat("Data loaded successfully!\n")
cat("Number of months:", nrow(goal_data), "\n")
cat("Columns:", paste(names(goal_data), collapse = ", "), "\n\n")

# ============================================================================
# STEP 2: CALCULATE MONTHLY METRICS
# ============================================================================

# Calculate throughput per unit (Price - Material Cost)
# This is the money generated after truly variable costs
goal_data$throughput_per_unit <- goal_data$price_per_unit - goal_data$material_cost_per_unit

# Calculate total monthly throughput (Units Sold × Throughput per Unit)
goal_data$monthly_throughput <- goal_data$units_sold * goal_data$throughput_per_unit

# Calculate monthly profit (Throughput - Operating Expense)
goal_data$monthly_profit <- goal_data$monthly_throughput - goal_data$operating_expense

# Calculate percent changes compared to previous month
# For month 1, these will be NA (no previous month to compare)
goal_data$pct_change_throughput <- c(NA, diff(goal_data$monthly_throughput) /
                                      head(goal_data$monthly_throughput, -1) * 100)

goal_data$pct_change_inventory <- c(NA, diff(goal_data$inventory_value) /
                                     head(goal_data$inventory_value, -1) * 100)

goal_data$pct_change_OE <- c(NA, diff(goal_data$operating_expense) /
                              head(goal_data$operating_expense, -1) * 100)

cat("Monthly metrics calculated successfully!\n\n")

# ============================================================================
# STEP 3: CREATE TRAFFIC LIGHT INDICATORS
# ============================================================================

# Initialize vectors for status indicators
throughput_status <- character(nrow(goal_data))
inventory_status <- character(nrow(goal_data))
oe_status <- character(nrow(goal_data))

# Loop through each month and assign traffic light colors
for (i in 1:nrow(goal_data)) {

  # THROUGHPUT STATUS (Higher is Better)
  if (is.na(goal_data$pct_change_throughput[i])) {
    throughput_status[i] <- "Yellow"  # First month baseline
  } else if (goal_data$pct_change_throughput[i] > 1) {
    throughput_status[i] <- "Green"   # Throughput increasing (GOOD!)
  } else if (goal_data$pct_change_throughput[i] >= -1) {
    throughput_status[i] <- "Yellow"  # Flat/stable
  } else {
    throughput_status[i] <- "Red"     # Throughput decreasing (BAD!)
  }

  # INVENTORY STATUS (Lower is Better)
  if (is.na(goal_data$pct_change_inventory[i])) {
    inventory_status[i] <- "Yellow"   # First month baseline
  } else if (goal_data$pct_change_inventory[i] < -1) {
    inventory_status[i] <- "Green"    # Inventory decreasing (GOOD!)
  } else if (goal_data$pct_change_inventory[i] <= 1) {
    inventory_status[i] <- "Yellow"   # Flat/stable
  } else {
    inventory_status[i] <- "Red"      # Inventory increasing (BAD!)
  }

  # OPERATING EXPENSE STATUS (Lower is Better)
  if (is.na(goal_data$pct_change_OE[i])) {
    oe_status[i] <- "Yellow"          # First month baseline
  } else if (goal_data$pct_change_OE[i] < -1) {
    oe_status[i] <- "Green"           # OE decreasing (GOOD!)
  } else if (goal_data$pct_change_OE[i] <= 1) {
    oe_status[i] <- "Yellow"          # Flat/stable
  } else {
    oe_status[i] <- "Red"             # OE increasing (BAD!)
  }
}

# Add status columns to dataframe
goal_data$throughput_status <- throughput_status
goal_data$inventory_status <- inventory_status
goal_data$oe_status <- oe_status

cat("Traffic light indicators created!\n\n")

# ============================================================================
# STEP 4: CREATE THE TRAFFIC LIGHT REPORT TABLE
# ============================================================================

# Create the report with key columns
traffic_report <- data.frame(
  Month = goal_data$month,
  Monthly_Throughput = sprintf("$%s", format(round(goal_data$monthly_throughput, 0),
                                             big.mark = ",", scientific = FALSE)),
  Inventory_Value = sprintf("$%s", format(round(goal_data$inventory_value, 0),
                                          big.mark = ",", scientific = FALSE)),
  Operating_Expense = sprintf("$%s", format(round(goal_data$operating_expense, 0),
                                           big.mark = ",", scientific = FALSE)),
  Throughput_Status = goal_data$throughput_status,
  Inventory_Status = goal_data$inventory_status,
  OE_Status = goal_data$oe_status
)

# Print the full report
cat("===============================================\n")
cat("   TRAFFIC LIGHT REPORT - 24 MONTH ANALYSIS   \n")
cat("===============================================\n\n")
print(traffic_report, row.names = FALSE)
cat("\n")

# ============================================================================
# STEP 6: CREATIVE ENHANCEMENT - System Health Dashboard
# ============================================================================

# Replace text with emojis for better visual impact
traffic_report_visual <- traffic_report
traffic_report_visual$Throughput_Status <- sapply(traffic_report_visual$Throughput_Status, function(x) {
  switch(x, "Green" = "🟢", "Yellow" = "🟡", "Red" = "🔴")
})
traffic_report_visual$Inventory_Status <- sapply(traffic_report_visual$Inventory_Status, function(x) {
  switch(x, "Green" = "🟢", "Yellow" = "🟡", "Red" = "🔴")
})
traffic_report_visual$OE_Status <- sapply(traffic_report_visual$OE_Status, function(x) {
  switch(x, "Green" = "🟢", "Yellow" = "🟡", "Red" = "🔴")
})

# Print visual version
cat("===============================================\n")
cat("   VISUAL TRAFFIC LIGHT REPORT (WITH EMOJIS)  \n")
cat("===============================================\n\n")
print(traffic_report_visual, row.names = FALSE)
cat("\n")

# Calculate summary statistics
green_count <- sum(goal_data$throughput_status == "Green", na.rm = TRUE) +
               sum(goal_data$inventory_status == "Green", na.rm = TRUE) +
               sum(goal_data$oe_status == "Green", na.rm = TRUE)

yellow_count <- sum(goal_data$throughput_status == "Yellow", na.rm = TRUE) +
                sum(goal_data$inventory_status == "Yellow", na.rm = TRUE) +
                sum(goal_data$oe_status == "Yellow", na.rm = TRUE)

red_count <- sum(goal_data$throughput_status == "Red", na.rm = TRUE) +
             sum(goal_data$inventory_status == "Red", na.rm = TRUE) +
             sum(goal_data$oe_status == "Red", na.rm = TRUE)

total_signals <- green_count + yellow_count + red_count

# Calculate System Health Score (0-100)
health_score <- round((green_count * 100 + yellow_count * 50 + red_count * 0) / total_signals, 1)

# Display summary
cat("===============================================\n")
cat("           SYSTEM HEALTH SUMMARY              \n")
cat("===============================================\n")
cat("🟢 Green Signals:", green_count, sprintf("(%.1f%%)\n", green_count/total_signals*100))
cat("🟡 Yellow Signals:", yellow_count, sprintf("(%.1f%%)\n", yellow_count/total_signals*100))
cat("🔴 Red Signals:", red_count, sprintf("(%.1f%%)\n", red_count/total_signals*100))
cat("───────────────────────────────────────────────\n")
cat("📊 Overall System Health Score:", health_score, "/ 100\n\n")

# Identify the worst month (most red signals)
month_red_counts <- apply(goal_data[, c("throughput_status", "inventory_status", "oe_status")],
                          1, function(x) sum(x == "Red", na.rm = TRUE))
worst_month_idx <- which.max(month_red_counts)
worst_month <- goal_data$month[worst_month_idx]

cat("⚠️  Worst Month:", worst_month, "with", month_red_counts[worst_month_idx], "red signals\n")

# Calculate rolling 3-month average for throughput
goal_data$throughput_3mo_avg <- NA
for (i in 3:nrow(goal_data)) {
  goal_data$throughput_3mo_avg[i] <- mean(goal_data$monthly_throughput[(i-2):i])
}

cat("📈 3-Month Rolling Average calculated for trend analysis\n\n")

# ============================================================================
# STEP 5: INTERPRETATION (Based on The Goal / Theory of Constraints)
# ============================================================================

cat("===============================================\n")
cat("         PLANT HEALTH DIAGNOSIS               \n")
cat("===============================================\n\n")

cat("Alex, here's what the data tells us about the plant's health:\n\n")

cat("The plant shows a System Health Score of", health_score, "out of 100, which indicates ")
if (health_score >= 70) {
  cat("relatively good operational health")
} else if (health_score >= 50) {
  cat("moderate concern requiring attention")
} else {
  cat("serious systemic problems")
}
cat(". ")

cat("However, as Jonah would remind us, we need to look at the DIRECTION of movement, not just the current state. ")

# Analyze recent trends (last 6 months)
recent_data <- tail(goal_data, 6)
recent_throughput_trend <- mean(recent_data$pct_change_throughput, na.rm = TRUE)
recent_inventory_trend <- mean(recent_data$pct_change_inventory, na.rm = TRUE)
recent_oe_trend <- mean(recent_data$pct_change_OE, na.rm = TRUE)

cat("Looking at the most recent six months, throughput is ")
if (recent_throughput_trend > 0) {
  cat("increasing ")
} else {
  cat("decreasing ")
}
cat(sprintf("(%.1f%% average change), ", recent_throughput_trend))

cat("inventory is ")
if (recent_inventory_trend < 0) {
  cat("decreasing ")
} else {
  cat("increasing ")
}
cat(sprintf("(%.1f%% average change), ", recent_inventory_trend))

cat("and operating expense is ")
if (recent_oe_trend < 0) {
  cat("decreasing ")
} else {
  cat("increasing ")
}
cat(sprintf("(%.1f%% average change). ", recent_oe_trend))

# Overall assessment
improving_metrics <- sum(c(recent_throughput_trend > 0,
                           recent_inventory_trend < 0,
                           recent_oe_trend < 0))

cat("\n\nJonah would likely say: ")
if (improving_metrics >= 2) {
  cat("'You're moving in the right direction, Alex. The system is improving, but don't get complacent. ")
  cat("Keep focusing on the constraint and making throughput flow.' ")
} else {
  cat("'Alex, the plant is still sick. You're not moving toward the goal. ")
  cat("Find the constraint, exploit it, subordinate everything else to it, and elevate it. ")
  cat("Until throughput is going up while inventory and operating expense are going down, you're just rearranging deck chairs.' ")
}

cat("The key insight from this analysis is that these three metrics are interconnected—")
cat("you cannot optimize one at the expense of the others. True improvement means simultaneous gains in all three.\n\n")

cat("===============================================\n")
cat("           END OF REPORT                      \n")
cat("===============================================\n")

