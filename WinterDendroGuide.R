library(shiny)
library(dplyr)

# Load the CSV file (ensure "plant_key.csv" is in your working directory)
# CSV should have columns: Step, NextStep, PlantName
plant_key <- read.csv("plant_key_dendro.csv", stringsAsFactors = FALSE, na.strings = c("NA", ""))

# List of bad dad jokes
tree_jokes <- c(
  "Why did the Quercus tree never feel lonely? Because it always had acorny sense of humor.",
  "Did you hear about the Pinus tree that got in trouble? It was pining for attention.",
  "Why was the Acer tree always invited to parties? It was sappy and sweet.",
  "What did the Fagus tree say when it couldn't solve the problem? I be-leaf I can't do it!",
  "Why did the Betula tree bring a ladder? To reach new heights in birch-ing.",
  "What do you call a Picea tree that tells jokes? A spruce-tacular comedian.",
  "Why did the Ulmus tree go to therapy? It had deep-rooted issues.",
  "Why don't Eucalyptus trees play hide and seek? Because they're easily koala-fied to find you.",
  "How did the Juglans tree do in school? It always walnut to study.",
  "Why was the Tilia tree always calm? It had a linden-ancy for zen.",
  "What did the Carpinus tree say to its friend? Don't be so hornbeam-headed.",
  "Why was the Salix tree so optimistic? It always willow-ed its troubles away.",
  "How did the Taxodium tree get to the top? It was cypress-istically motivated.",
  "Why did the Prunus tree start a blog? To share its cherry thoughts.",
  "What did the Populus tree say when it was famous? I'm pop-ular, of course!",
  "Why did the Fraxinus tree become an artist? It had an ash-piration for greatness.",
  "Why did the Larix tree get promoted? It was always larically inclined.",
  "How did the Abies tree make friends? By being fir-ly approachable.",
  "What did the Ginkgo tree say to the gym instructor? I want to leaf-fit!",
  "Why don't Cedrus trees gossip? They cedar respect for privacy.",
  "What did the Magnolia tree say to the other tree? You magnolia best!",
  "Why did the Olea tree always stay calm? Because it was olive-ing its best life.",
  "Why was the Ficus tree always happy? It had a positive leaf-style.",
  "What did the Quercus tree say to its crush? I oak for your love.",
  "Why did the Buxus tree become a DJ? It was the master of hedgbeats.",
  "Why did the Eucalyptus tree go to school? To get a higher koalafication.",
  "What did the Pyrus tree say when it was upset? I'm pearly hanging on!",
  "Why did the Laurus tree never get lost? It always had a bay-leaf.",
  "What did the Ilex tree say to its friend? Holly me close!",
  "Why did the Populus tree start a band? It wanted to be a popstar.",
  "What did the Castanea tree say when it was tired? I'm chestnut-ing it today.",
  "Why did the Pinus tree get a promotion? It was pining for success.",
  "What did the Betula tree say to cheer up its friend? Birch, please!",
  "Why was the Ginkgo tree always so smart? It had a lot of leaf-learning.",
  "What did the Taxus tree say when it was excited? Yew've got this!",
  "Why did the Salix tree become an artist? It was willowing in creativity.",
  "What did the Acer tree say to motivate its friend? Maple-ieve in yourself!",
  "Why did the Prunus tree become a writer? It had a cherry passion for words.",
  "What did the Carpinus tree say to its mentor? You're my role-model, hornbeam!",
  "Why did the Abies tree join the gym? It wanted to be fit and fir-m."
)

# Function to trace keying steps back from the final plant step
trace_keying_steps <- function(plant_name, plant_key) {
  # Find rows where the PlantName matches and NextStep is NA (i.e. final step)
  final_rows <- filter(plant_key, PlantName == plant_name & (is.na(NextStep) | NextStep == ""))
  if(nrow(final_rows) == 0) {
    return(NULL)  # No matching plant found
  }
  
  # Take the first matching final row
  final_row <- final_rows[1, ]
  current_step <- final_row$Step
  keying_steps <- c(current_step)  # Start with the final step
  
  # Backtrack: Look for the row whose NextStep equals the current step.
  repeat {
    prev_row <- filter(plant_key, NextStep == current_step)
    if(nrow(prev_row) == 0) break  # Stop if no previous step is found
    current_step <- prev_row$Step[1]  # Assume unique path
    keying_steps <- c(current_step, keying_steps)  # Prepend the step
  }
  
  return(keying_steps)
}

# Define the Shiny UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f8f9fa;
        font-family: Arial, sans-serif;
        font-size: 18px;
      }
      .title {
        color: #007bff;
        text-align: center;
        margin-bottom: 20px;
        font-size: 32px;
      }
      .sidebar {
        background-color: #ffffff;
        padding: 20px;
        border-radius: 5px;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
        font-size: 18px;
      }
      .main {
        background-color: #ffffff;
        padding: 20px;
        border-radius: 5px;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
        font-size: 18px;
      }
      .btn-primary {
        background-color: #007bff;
        border-color: #007bff;
        font-size: 18px;
        padding: 10px 20px;
      }
      .btn-primary:hover {
        background-color: #0056b3;
        border-color: #0056b3;
      }
      table {
        width: 100%;
        margin-top: 20px;
        font-size: 18px;
      }
      th, td {
        padding: 15px;
        text-align: center;
        border: 1px solid #dee2e6;
        font-weight: bold;
      }
      .joke {
        font-size: 14px;
        color: #6c757d;
        text-align: center;
        margin-top: 20px;
      }
      @media (max-width: 768px) {
        .sidebar, .main {
          padding: 10px;
        }
        .btn-primary {
          width: 100%;
          padding: 15px;
        }
      }
    "))
  ),
  titlePanel(h1("Reverse Plant Key Lookup", class = "title")),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      textInput("plant_name", "Enter Genus Name (e.g., Pinus):", value = "Pinus"),
      actionButton("submit", "Find Keying Steps", class = "btn btn-primary")
    ),
    mainPanel(
      class = "main",
      h3("Keying Steps:"),
      tableOutput("steps_table"),
      div(textOutput("joke"), class = "joke")
    )
  )
)

# Define the Shiny server logic
server <- function(input, output) {
  steps_vec <- eventReactive(input$submit, {
    trace_keying_steps(input$plant_name, plant_key)
  })
  
  output$steps_table <- renderTable({
    vec <- steps_vec()
    if (is.null(vec)) {
      return(data.frame(Message = "Plant not found in key."))
    }
    # Set the number of columns for display (change num_cols as desired)
    num_cols <- 3  
    num_steps <- length(vec)
    num_rows <- ceiling(num_steps / num_cols)
    
    # Create an empty matrix and fill it in column-major order (top-to-bottom, then left-to-right)
    mat <- matrix("", nrow = num_rows, ncol = num_cols)
    mat[1:num_steps] <- vec
    
    # Convert matrix to data frame for display
    as.data.frame(mat)
  }, rownames = FALSE, colnames = FALSE)
  
  output$joke <- renderText({
    input$submit
    isolate({
      sample(tree_jokes, 1)
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
