library(ggplot2)
library(ggvis)
library(shiny)
library(dplyr)
library(tidyr)


ui <- navbarPage("BABY PREDICTIONS",

	tabPanel("Date of Birth", 
	
		fluidRow(
			tags$div(class="title", titlePanel("Predictions for Date of Birth"))
		),
	
		fluidRow(
			column(10, offset = 1,
				plotOutput(outputId = "plot_date")
			)
		),	
	
		hr(),
	
		fluidRow(
			column(6, offset = 3,
				sliderInput(inputId = "binwidth_date",
				width = "100%",
				label = "Number of days to group predictions by:",
				min = 1,
				max = 7,
				value = 1)
			)
		)
	
	),
	
	tabPanel("Time of Birth", 
	
		fluidRow(
			tags$div(class="title", titlePanel("Predictions for Time of Birth"))
		),
	
		fluidRow(
			column(10, offset = 1,
				plotOutput(outputId = "plot_time")
			)
		),	
	
		hr(),
		
		fluidRow(
			column(6, offset = 3,
				shinyWidgets::sliderTextInput(inputId = "binwidth_time", 
  					label = "Number of hours to group predictions by:", 
  					choices = c(1, 2, 3, 6, 12),
  					grid = TRUE,
					width = "100%"
				)
			)
		)
	
	),
	
	tabPanel("Weight & Height", 
	
		fluidRow(
			tags$div(class="title", titlePanel("Predictions for Weight and Height at Birth"))
		),
	
		fluidRow(
			column(10, offset = 1,
				plotOutput(outputId = "plot_weight_height")
			)
		),	
	
		hr(),
		
		fluidRow(
			column(6, offset = 3
			)
		)
	
	),
	
	tabPanel("Gender", 
	
		fluidRow(
			tags$div(class="title", titlePanel("Predictions for Gender"))
		),
	
		fluidRow(
			column(10, offset = 1,
				plotOutput(outputId = "plot_gender")
			)
		),	
	
		hr(),
		
		fluidRow(
			column(6, offset = 3
			)
		)
	
	),
	
	tags$head(
				tags$style(
					"
					@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
					
					.title {
						margin: auto; 
						width: 80%; 
						text-align: center; 
						font-family: 'Lobster', 
						cursive; 
						font-weight: 500; 
						color: #48ca3b; 
						line-height: 1.1;}
						
					.navbar-brand .navbar-default {
						color: 'red'
					}
					"
				)
			)
	
	
)

server <- function(input, output) {
	
	predictions <- read.csv("baby_predictions_df.csv", header = TRUE)
	predictions$date_of_birth <- as.Date(predictions$date_of_birth, format = "%m/%d/%y")
	predictions$time_of_birth <- as.POSIXct(predictions$time_of_birth, format = "%H:%M")
	
	today <- toString(Sys.Date())
	tomorrow <- toString(Sys.Date() + 1)
	lim_time <- " 00:00"
	min_lim <- paste(today, lim_time)
	max_lim <- paste(tomorrow, lim_time)
	lims <- as.POSIXct(strptime(c(min_lim, max_lim), format = "%Y-%m-%d %H:%M"))
	
	gender_colors = c("boy" = "skyblue1", "girl" = "pink")

	output$plot_date <- renderPlot({
		
		binwidth <- input$binwidth_date
		
		predictions %>% ggplot(aes(x = date_of_birth)) + geom_vline(xintercept = as.numeric(predictions$date_of_birth[1]), color = "yellow", size = 6) + geom_vline(xintercept = as.numeric(predictions$date_of_birth[4]), color = "lightgreen", size = 6) + geom_bar(alpha=0.8, fill = "white", color = "black", binwidth = binwidth) + geom_density(aes(y=..scaled..), color = "darkgrey", fill = "yellow", alpha = 0.6) + scale_x_date(date_labels="%b %d",date_breaks  ="2 days", expand = c(0.05,0.05)) + labs(x = "\nDate of Birth", y = "Number of Predictions\n") + annotate("text", x = predictions$date_of_birth[1], y = 4, label = "Actual Birth Date (June 24)", size = 5, family = "Palatino", angle = -90) + annotate("text", x = predictions$date_of_birth[4], y = 5, label = "Due Date", size = 5, family = "Palatino", angle = -90) + theme(axis.title = element_text(size = rel(1.75)), axis.text.x = element_text(size = rel(1.4)), plot.margin = unit(c(0,0.75,0,0.75), "cm"))
			
	})	
	
	output$plot_time <- renderPlot({
		
			binwidth <- input$binwidth_time
		
		predictions %>% ggplot(aes(x = time_of_birth)) + geom_vline(xintercept = as.numeric(predictions$time_of_birth[4]), color = "yellow", size = 6) + geom_histogram(alpha=0.8, fill = "white", color = "black", bins = (24/binwidth)) + geom_density(aes(y=..scaled..), color = "yellow", fill = "yellow", alpha = 0.6) + scale_x_datetime(date_label = "%H:%M", limits = lims, date_breaks = "3 hours", expand = c(0,0)) + labs(x = "\nTime of Birth (24h)", y = "Number of Predictions\n") + theme(axis.title = element_text(size = rel(1.75)), axis.text.x = element_text(size = rel(1.4)), plot.margin = unit(c(0,0.75,0,0.75), "cm")) + geom_vline(xintercept = as.numeric(predictions$time_of_birth[4]), color = "yellow", size = 6, alpha = 0.8) + annotate("text", x = predictions$time_of_birth[4], y = 2.5, label = "Actual Birth Time (14h35)", size = 5, family = "Palatino", angle = -90)
			
	})	
	 
	
	output$plot_weight_height <- renderPlot({
		
		predictions %>% filter(gender != "") %>% ggplot(aes(x = weight_kgs, y = height_cm)) + annotate("rect", xmin = 2.2, xmax = 4.2, ymin = -Inf, ymax = Inf, alpha = 0.1) + annotate("rect", xmin = 2.7, xmax = 3.7, ymin = -Inf, ymax = Inf, alpha = 0.2) + annotate("rect", xmin = -Inf, xmax = Inf, ymin = 44.0, ymax = 54.0, alpha = 0.1) + annotate("rect", xmin = -Inf, xmax = Inf, ymin = 46.5, ymax = 51.5, alpha = 0.2) + geom_vline(xintercept = 3.2, alpha = 0.6) + geom_hline(yintercept = 49.0, alpha = 0.6) + geom_point(aes(fill = gender), shape = 21, size = 3, alpha = 0.8) + scale_x_continuous(limits = c(2.1, 4.3), breaks = c(2.25, 2.5, 2.75, 3.0, 3.25, 3.5, 3.75, 4.0, 4.25), "\nWeight (Kg)", sec.axis = sec_axis(~ . * 2.2, name = "Weight (lbs)\n", breaks = c(5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9))) + scale_y_continuous(limits = c(25, 60), breaks = c(25, 30, 35, 40, 45, 50, 55, 60), "Height (cm)\n", sec.axis = sec_axis(~ . / 2.54 , name = "Height (in)\n", breaks = c(10, 12.5, 15, 17.5, 20, 22.5))) + scale_fill_manual(values = gender_colors) + annotate("text", x = 4.0, y = 49.50, label = "National Mean ± 1 SD and ± 2 SD", size = 3, family = "Palatino") + annotate("text", x = 3.220, y = 38.6, label = "National Mean ± 1 SD and ± 2 SD", size = 3, family = "Palatino", angle = -90) + labs(fill = "Gender") + theme(axis.title = element_text(size = rel(1.75)), axis.text.x = element_text(size = rel(1.4)), axis.text.y = element_text(size = rel(1.4)), plot.margin = unit(c(0,0.75,0.5,0.75), "cm")) + guides(fill = FALSE) + geom_point(aes(x=3.175, y=51.1175), color="yellow", shape = 21, size = 3) + annotate("text", x = 3.48, y = 35, label = "Yellow Circle\n---------------\nActual Weight =\n(3.175 kg/7.0 lb)\nActual Height =\n(51.1 cm/20.1 in)", size = 5, family = "Palatino", color = "lightyellow1", fontface = 2)
			
	})	
	
	output$plot_gender <- renderPlot({
		
		predictions %>% filter(gender != "") %>% group_by(gender) %>% summarize(n = n()) %>% mutate(proportions = n / sum(n), percent = paste(round((n / sum(n)) * 100, 1), "%", sep = "")) %>% ggplot(aes(x = "", y = proportions, fill = gender)) + geom_bar(stat = "identity", width = 1, color = "white") + coord_polar(theta = "y", start = 0) + scale_fill_manual(labels = c("Boy", "Girl"), values=c("skyblue1", "pink")) + geom_text(aes(label = percent), family = "Palatino", color = "white", size = 10, position = position_stack(vjust = 0.5)) + theme_void() + theme(legend.position = "bottom", legend.text = element_text(size = 20, family = "Palatino"), legend.key.size = unit(2,"line")) + labs(fill = "")
		
	})


	
}

shinyApp(ui = ui, server = server)