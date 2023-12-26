# set Project Path
setwd("C:\\Users\\rtx30\\OneDrive\\Documents\\Titenic Analysis R-OW")
print(getwd())



# Read The .csv File
data_one <- read.csv("titanic_all_data.csv")

perform_analysis <- function(choice) {
  switch(choice,
         "View Data Set" = {
           
           data_one <- read.csv("titanic_all_data.csv")
           View(data_one)
           
         },
    "1" = {
      survived_counts <- table(data_one$Survived)
      percentage_survived <- (survived_counts / sum(survived_counts)) * 100

      print("Survived Counts")
      print(survived_counts)

      print("Percentage of Survived")
      print(percentage_survived)

      # Create labels for the pie chart
      labels <- c("Did Not Survive", "Survived")
      colors <- c("red", "green")
      pie(survived_counts, labels = labels, col = colors, main = "Survival Distribution on the Titanic")
      legend("topright", labels, cex = 0.5, fill = colors)
    },
    "2" = {
      # Determine the distribution of passengers in each class (1st, 2nd, 3rd)
    class_survival_rates <- tapply(data_one$Survived, data_one$Pclass, function(x) sum(x) / length(x) * 100)

      cat("Survival Rate by Passenger class :  \n")
      for (pclass in unique(data_one$Pclass))
      {
        cat("Class", pclass, ":", class_survival_rates[pclass], "%\n")
      }

      # Box Plot
      boxplot(data_one$Pclass, data = data_one$Survived, xlab = "Passenger Class", ylab = "Survived", main = "Survived Rates by Passenger class")
    },
    "3" = {
     average_fare_survivors <- mean(data_one$Fare[data_one$Survived == 1])

      average_fare_not_survivors <- mean(data_one$Fare[data_one$Survived == 0])

      cat("Average Fare For Survived Pessengers : $", round(average_fare_survivors, 2), "\n")
      cat("Average Fare For Non Survived Pessengers : $", round(average_fare_not_survivors, 2), "\n")

      summary(data_one$Fare)

      # Create a histogram of fare distribution
      hist(data_one$Fare, 
           main = "Distribution of Fares Paid by Passengers",
           xlab = "Fare",
           ylab = "Frequency",
           col = "skyblue",
           border = "black",
           breaks = 20)  
      

      
    },
    "4" = {
      # Extract and analyze titles (e.g., Mr., Mrs., Miss) from passenger names.
      data_one$Title <- gsub("(.*, )|(\\..*)", "", data_one$Name)

      unique_titles <- table(data_one$Title)
      print("Unique Titles : ")
      print(unique_titles)

      top_titles <- head(sort(unique_titles, decreasing = TRUE), 4)
      print("Top 4 Most Common Titles")
      print(top_titles)

      top_titles_data <- data.frame(
        Title = names(top_titles),
        Count = as.numeric(top_titles)
      )

      # Create a bar chart 
      barplot(top_titles_data$Count, names.arg = top_titles_data$Title, col = "yellow",
              main = "Top 4 Most Common Titles", xlab = "Title", ylab = "Count", cex.names = 0.8)
      
    },
    "5" = {
      male_pessengers <- data_one[data_one$Sex == "male", ]
      num_male_pessengers <- nrow(male_pessengers)


      female_pessengers <- data_one[data_one$Sex == "female", ]
      num_female_pessengers <- nrow(female_pessengers)

      male_survived_rate <- sum(data_one$Survived[data_one$Sex == "male"]) / sum(data_one$Sex == "male") * 100

      female_survival_rate <- sum(data_one$Survived[data_one$Sex == "female"]) / sum(data_one$Sex == "female") * 100

      cat("Number of Female passengers : ", num_female_pessengers, "\n", "Survival Rates For Female : ", female_survival_rate, "\n")
      cat("Number of Male passengers : ", num_male_pessengers, "\n", "Survival Rates For Male : ", male_survived_rate, "\n")
    },
    "6" = {
      ticket_groups <- split(data_one$Name, data_one$Ticket)

      for (ticket in names(ticket_groups)) {
        if (length(ticket_groups[[ticket]]) > 1) {
          cat("Ticket Number:", ticket, "\n")
          cat("Passengers with the same ticket:\n")
          cat(ticket_groups[[ticket]], sep = "\n")
          cat("\n\n")
        }
      }

      # Count the number of passengers with the same ticket number
      ticket_counts <- sapply(ticket_groups, length)
      
      # Create a data frame with Ticket Number and Passenger Count
      ticket_data <- data.frame(TicketNumber = as.numeric(names(ticket_counts)), PassengerCount = ticket_counts)
      
      # Sort the data by Ticket Number
      ticket_data <- ticket_data[order(ticket_data$TicketNumber), ]
      
      # Create a line chart using plot()
      plot(ticket_data$TicketNumber, ticket_data$PassengerCount, type = "o", col = "red",
           xlab = "Ticket Number", ylab = "Number of Passengers",
           main = "Number of Passengers with the Same Ticket Number")
      
      
      
    },
    "7" = {
      survival_rates <- tapply(data_one$Survived, data_one$SibSp, function(x) sum(x) / length(x) * 100)
      cat("Survival Rates by Number of Siblings/Spouses (SibSp):\n")
      for (num_sibsp in unique(data_one$SibSp)) {
        cat("SibSp", num_sibsp, ":", survival_rates[num_sibsp], "%\n")
      }
      siblings_spouses_survived <- table(data_one$SibSp[data_one$Survived == 1])
      cat("\nNumber of Siblings/Spouses who Survived:\n")
      print(siblings_spouses_survived)
    },
    "8" = {
      data_one$FamilySize <- data_one$Parch + data_one$SibSp
      survival_rates_family_size <- tapply(data_one$Survived, data_one$FamilySize, function(x) sum(x) / length(x) * 100)

      cat("Survival Rates by Family Size:\n")
      print(survival_rates_family_size)

      family_size_survival_data <- data.frame(
        FamilySize = as.numeric(names(survival_rates_family_size)),
        SurvivalRate = as.numeric(survival_rates_family_size)
      )
      
      # Create a pie chart
      pie(family_size_survival_data$SurvivalRate, labels = family_size_survival_data$FamilySize, col = rainbow(length(family_size_survival_data$FamilySize)))
      
      # Add a title
      title("Survival Rates by Family Size")
      
      # Optionally, add a legend
      legend("topright", legend = family_size_survival_data$FamilySize, title = "Family Size", fill = rainbow(length(family_size_survival_data$FamilySize)))
             
    },
    "9" = {
      library(ggplot2)
      embarked_distribution <- ggplot(data_one, aes(x = Embarked, fill = Embarked)) +
        geom_bar() +
        labs(
          title = "Distribution of Passengers by Embarked Port",
          x = "Port of Embarkation", y = "Count"
        ) +
        scale_fill_manual(values = c("C" = "blue", "Q" = "green", "S" = "red"), name = "Port of Embarkation") +
        theme_minimal()
      print(embarked_distribution)
      
      survival_rates_embarked <- tapply(data_one$Survived, data_one$Embarked, function(x) sum(x) / length(x) * 100)
      
      cat("Survival Rates by Port of Embarkation:\n")
      print(survival_rates_embarked)
      
    },
    "10" = {
      cat("Exiting analysis program.\n")
    },
    {
      cat("Invalid choice. Please select a valid option.\n")
    }
  )
}



get_user_input_and_execute <- function() {
  while (TRUE) {
    cat("Select an analysis option:\n")
    cat("View Data Set\n")
    cat("1. Analyze survived\n")
    cat("2. Analyze Pclass\n")
    cat("3. Analyze fare\n")
    cat("4. Analyze name\n")
    cat("5. Analyze gender\n")
    cat("6. Analyze ticket\n")
    cat("7. Analyze Siblings\n")
    cat("8. Analyze Prents/Children\n")
    cat("9. Analyze Embarked\n")
    cat("10. Exit\n")

    choice <- readline(prompt = "Enter your choice: ")

    # Execute the selected analysis based on user's choice
    perform_analysis(choice)

    if (choice == "10") {
      break
    }
  }
}

get_user_input_and_execute()

  