#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(tidyverse)
library(infotheo)
library(GGally)
library(tidymodels)
library(ggridges)
library(plotly)

#---- 1. Preliminaries ----

setwd("~/studium/8 semester/oznal/project")
schema <- read_csv("data/survey_results_schema.csv", show_col_types = FALSE)
data <- read_csv("data/survey_results_public.csv", show_col_types = FALSE)
data <- data %>%
    drop_na(ConvertedCompYearly) %>%
    filter(ConvertedCompYearly > 0) %>%
    filter(DevType != 'Student',
           DevType != 'Retired',
           DevType != 'Other (please specify):' ) %>%
    filter(Employment != 'Retired',
           Employment != 'Not employed',
           Employment != 'I prefer not to say' )
#---- 2. Plot definitions ----

bar_plot <- function(x, name) {
    x %>%
        select(all_of(name)) %>%
        ggplot(mapping = aes(x = .data[[name]])) +
        geom_bar(fill = "grey40") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("Distribution of", name), x = name)
}

histogram <- function(x, name) {
    x %>%
        select(all_of(name)) %>%
        ggplot(mapping = aes(x = .data[[name]])) +
        geom_histogram(fill = "steelblue", bins = 50) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(title = paste("Distribution of", name), x = name)
}

multichoice_histogram <- function(x, name) {
    x %>%
        select(all_of(name)) %>%
        separate_longer_delim(everything(), delim = ";") %>%
        ggplot(mapping = aes(x = .data[[name]])) +
        geom_bar(fill = "orange2") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("Distribution of", name), x = name)
}

#---- 3. Vars groups ----
multichoice_vars <- c("EmploymentAddl", "LearnCode", "AILearnHow",
                         "SO_Dev_Content", "AIFrustration", "AIAgent_Uses",
                         "AgentUsesGeneral", "AIAgentKnowledge",
                         "AIAgentOrchestration", "AIAgentObserveSecure",
                         "AIAgentExternal", "AIHuman")
rate_column_prefixes <- c("TechEndorse_", "JobSatPoints_", "TechOppose_",
                          "SO_Actions_")
tech_usage_column_prefixes <- c("Language", "Database", "Platform", "Webframe",
                                "DevEnvs", "AIModels", "SOTags", "CommPlatform",
                                "OfficeStackAsync", "DevEnv", "OpSys", "OfficeStack")
double_multichoice_prefixes <- c("AITool", "AIAgentImpact", "AIAgentChallenges")
nominal_columns <- c("MainBranch", "EdLevel", "Employment",
                     "DevType", "ICorPM", "RemoteWork",
                     "Industry", "Country")
ordinal_columns <- c("Age", "OrgSize", "AIThreat", "SOAccount", "SOVisitFreq",
                     "SODuration",
                     "AISelect", "AISent", "AIAcc", "AIComplex", "AIAgents","AIAgentChange",
                     "JobSat")
numeric_columns <- c("WorkExp", "YearsCode", "ToolCountWork")
tech_usage_column_prefixes <- c("LanguageHaveWorkedWith", "DatabaseHaveWorkedWith", "PlatformHaveWorkedWith",
                                "WebframeHaveWorkedWith",  "DevEnvsHaveWorkedWith", "AIModelsHaveWorkedWith",
                                "CommPlatformHaveWorkedWith", "OfficeStackAsyncHaveWorkedWith", "OpSysProfessional use")

selected_features <- c(nominal_columns, ordinal_columns, numeric_columns,tech_usage_column_prefixes,"ConvertedCompYearly")

#---- 4. Preparing data ----
data_filtered <- data %>%
    select(all_of(selected_features))
lower_bound <- quantile(data_filtered$ConvertedCompYearly, 0.025, na.rm = TRUE)
upper_bound <- quantile(data_filtered$ConvertedCompYearly, 0.975, na.rm = TRUE)
data_cleaned <- data_filtered %>%
    filter(ConvertedCompYearly >= lower_bound & ConvertedCompYearly <= upper_bound)
upper_bound_w <- quantile(data_filtered$WorkExp, 0.99, na.rm = TRUE)
upper_bound_y <- quantile(data_filtered$YearsCode, 0.99, na.rm = TRUE)
upper_bound_t <- quantile(data_filtered$ToolCountWork, 0.98, na.rm = TRUE)
data_cleaned <- data_cleaned %>%
    mutate(
        WorkExp = if_else(WorkExp > upper_bound_w, upper_bound_w, WorkExp),
        YearsCode = if_else(YearsCode > upper_bound_y, upper_bound_y, YearsCode),
        ToolCountWork = if_else(ToolCountWork > upper_bound_t, upper_bound_t, ToolCountWork)
    )

#---- 5. Recipes ----
country_list <- c("Australia", "Austria", "Brazil", "Canada", "Czech Republic", "France", "Germany", "India", "Italy", "Netherlands", "Poland", "Portugal", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom of Great Britain and Northern Ireland", "United States of America")
nominal_recipe <- roles_recipe %>%
    step_factor2string(has_role("nominal")) %>%
    step_impute_mode(ICorPM, RemoteWork) %>%
    step_mutate(
        EdLevel = replace_na(EdLevel, "Primary/elementary school")
    ) %>%
    step_unknown(Industry, new_level = "None") %>%
    step_mutate(
        MainBranch = fct_recode(MainBranch,
                                "Developer" = "I am a developer by profession",
                                "Student" = "I am learning to code",
                                "Occasional" = "I am not primarily a developer, but I write code sometimes as part of my work/studies",
                                "Hobbyist" = "I code primarily as a hobby",
                                "ExDeveloper" = "I used to be a developer by profession, but no longer am",
                                "Adjacent" = "I work with developers or my work supports developers but am not a developer by profession"
        ),
        EdLevel = fct_recode(EdLevel,
                             "Associate" = "Associate degree (A.A., A.S., etc.)",
                             "Bachelor" = "Bachelor’s degree (B.A., B.S., B.Eng., etc.)",
                             "Master" = "Master’s degree (M.A., M.S., M.Eng., MBA, etc.)",
                             "Other" = "Other (please specify):",
                             "Primary" = "Primary/elementary school",
                             "Professional" = "Professional degree (JD, MD, Ph.D, Ed.D, etc.)",
                             "Secondary" = "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)",
                             "Dropout" = "Some college/university study without earning a degree",
        ),
        Employment = fct_recode(Employment,
                                "Independent" = "Independent contractor, freelancer, or self-employed",
        ),
        DevType = fct_recode(DevType,
                             "Academic" = "Academic researcher",
                             "MLEngineer" = "AI/ML engineer",
                             "Scientist" = "Applied scientist",
                             "Architect" = "Architect, software or solutions",
                             "CloudEngineer" = "Cloud infrastructure engineer",
                             "Cybersecurity" = "Cybersecurity or InfoSec professional",
                             "DataEngineer" = "Data engineer",
                             "BusinessAnalyst" = "Data or business analyst",
                             "DataScientist" = "Data scientist",
                             "DatabaseAdmin" = "Database administrator or engineer",
                             "AIDeveloper" = "Developer, AI apps or physical AI",
                             "Backend" = "Developer, back-end",
                             "Desktop" = "Developer, desktop or enterprise applications",
                             "Embedded" = "Developer, embedded applications or devices",
                             "Frontend" = "Developer, front-end",
                             "Fullstack" = "Developer, full-stack",
                             "Game" = "Developer, game or graphics",
                             "Mobile" = "Developer, mobile",
                             "QA" = "Developer, QA or test",
                             "DevOps" = "DevOps engineer or professional",
                             "EngineeringManager" = "Engineering manager",
                             "FinancialAnalyst" = "Financial analyst or engineer",
                             "Founder" = "Founder, technology or otherwise",
                             "ProductManager" = "Product manager",
                             "ProjectManager" = "Project manager",
                             "Executive" = "Senior executive (C-suite, VP, etc.)",
                             "Support" = "Support engineer or analyst",
                             "SysAdmin" = "System administrator",
                             "Designer" = "UX, Research Ops or UI design professional",
        ),
        RemoteWork = fct_recode(RemoteWork,
                                "Hybrid" = "Hybrid (some in-person, leans heavy to flexibility)",
                                "Hybrid" = "Hybrid (some remote, leans heavy to in-person)",
                                "InPerson" = "In-person",
                                "Remote" = "Remote",
                                "Flexible" = "Your choice (very flexible, you can come in when you want or just as needed)",
        )
    ) %>%
    # convert ICorPM to IsPM (is People Manager)
    step_rename(IsPM = ICorPM) %>%
    step_mutate(IsPM = if_else(IsPM == "People manager", 1L, 0L)) %>%
    step_mutate(
        Country = factor(
            if_else(
                as.character(Country) %in% country_list,
                Country,
                "Other"
            ),
            levels = c(country_list, "Other")
        )
    )

ordinal_recipe <- nominal_recipe %>%
    step_mutate(
        Age = Age %>%
            factor(levels = c("Prefer not to say",
                              "18-24 years old",
                              "25-34 years old",
                              "35-44 years old",
                              "45-54 years old",
                              "55-64 years old",
                              "65 years or older"),
                   ordered = TRUE)
    ) %>%
    step_mutate(
        OrgSize = OrgSize %>%
            factor(levels = c(
                "None",
                "Just me - I am a freelancer, sole proprietor, etc.",
                "Less than 20 employees",
                "20 to 99 employees",
                "100 to 499 employees",
                "500 to 999 employees",
                "1,000 to 4,999 employees",
                "5,000 to 9,999 employees",
                "10,000 or more employees",
                "I don't know"),
                ordered = TRUE) %>%
            fct_recode("None" = "I don't know") %>%
            fct_na_value_to_level("None")
    ) %>%
    step_mutate(
        AIThreat = AIThreat %>%
            factor(ordered = TRUE,
                   levels = c("No", "I'm not sure", "Yes")
            ) %>%
            fct_recode("No" = "I'm not sure") %>%
            fct_na_value_to_level("No") %>%
            fct_relevel(c("No",
                          "Yes"))
    ) %>%
    step_mutate(
        SOAccount = SOAccount %>%
            factor(levels = c("No",
                              "Not sure/can't remember",
                              "Yes"),
                   ordered = TRUE) %>%
            fct_na_value_to_level("Not sure/can't remember")
    ) %>%
    step_mutate(
        SOVisitFreq = SOVisitFreq %>%
            factor(ordered = TRUE,
                   levels = c("Infrequently, less than once per year",
                              "Less than once every 2 - 3 months",
                              "Less than once per month or monthly",
                              "A few times per month or weekly",
                              "A few times per week",
                              "Daily or almost daily",
                              "Multiple times per day")) %>%
            fct_na_value_to_level("Infrequently, less than once per year")
    ) %>%
    step_mutate(
        SODuration = SODuration %>%
            factor(ordered = TRUE, levels = c("I don't use Stack Overflow",
                                              "Less than one year",
                                              "Between 1 and 3 years",
                                              "Between 3 and 5 years",
                                              "Between 5 and 10 years",
                                              "Between 10 and 15 years",
                                              "More than 15 years, or since Stack Overflow started in 2008")) %>%
            fct_na_value_to_level("I don't use Stack Overflow")
    ) %>%
    step_mutate(
        AISelect = AISelect %>%
            factor(ordered = TRUE, levels = c("No, and I don't plan to",
                                              "No, but I plan to soon",
                                              "Yes, I use AI tools monthly or infrequently",
                                              "Yes, I use AI tools weekly",
                                              "Yes, I use AI tools daily")) %>%
            fct_na_value_to_level("Yes, I use AI tools monthly or infrequently")
    ) %>%
    step_mutate(
        AISent = AISent %>%
            factor(ordered = TRUE, levels = c("Very unfavorable",
                                              "Unfavorable",
                                              "Unsure",
                                              "Indifferent",
                                              "Favorable",
                                              "Very favorable")) %>%
            fct_na_value_to_level("Indifferent")
    ) %>%
    step_mutate(
        AIAcc = AIAcc %>%
            factor(ordered = TRUE, levels = c("Highly distrust",
                                              "Highly trust",
                                              "Neither trust nor distrust",
                                              "Somewhat distrust",
                                              "Somewhat trust")) %>%
            fct_na_value_to_level("Neither trust nor distrust")
    ) %>%
    step_mutate(
        AIComplex = AIComplex %>%
            factor(ordered = TRUE, levels = c("Very poor at handling complex tasks",
                                              "Bad at handling complex tasks",
                                              "I don't use AI tools for complex tasks / I don't know",
                                              "Neither good or bad at handling complex tasks",
                                              "Good, but not great at handling complex tasks",
                                              "Very well at handling complex tasks")) %>%
            fct_na_value_to_level("I don't use AI tools for complex tasks / I don't know")
    ) %>%
    step_mutate(
        AIAgents = AIAgents %>%
            factor(ordered = TRUE, levels = c("No, and I don't plan to",
                                              "No, but I plan to",
                                              "No, I use AI exclusively in copilot/autocomplete mode",
                                              "Yes, I use AI agents at work daily",
                                              "Yes, I use AI agents at work monthly or infrequently",
                                              "Yes, I use AI agents at work weekly")) %>%
            fct_na_value_to_level("No, and I don't plan to")
    ) %>%
    step_mutate(
        AIAgentChange = AIAgentChange %>%
            factor(ordered = TRUE, levels = c("Not at all or minimally",
                                              "No, but my development work has changed somewhat due to non-AI factors",
                                              "No, but my development work has significantly changed due to non-AI factors",
                                              "Yes, somewhat",
                                              "Yes, to a great extent")) %>%
            fct_na_value_to_level("Not at all or minimally")
    ) %>%
    step_impute_median(JobSat)

numeric_recipe <- ordinal_recipe %>%
    step_impute_median(has_role("numeric"))
final_recipe <- numeric_recipe %>%
    step_rm(-has_role("numeric"), -has_role("ordinal"), -has_role("nominal"), -has_role("outcome"))


corr_data = list(
    "onehot" = final_recipe %>%
        step_rm(-all_outcomes(), -has_role("nominal")) %>%
        step_dummy(has_role("nominal"), one_hot = TRUE) %>%
        prep() %>%
        bake(new_data = NULL),
    "integer" = final_recipe %>%
        step_rm(-all_outcomes(), -has_role("nominal")) %>%
        step_integer(has_role("nominal")) %>%
        prep() %>%
        bake(new_data = NULL),
    "ordinal" = final_recipe %>%
        step_rm(-has_role("ordinal"), -all_outcomes()) %>%
        step_ordinalscore(has_role("ordinal"), -JobSat) %>%
        prep() %>%
        bake(new_data = NULL)
)

#---- 6. Pages ----

distributions_page <-
    nav_panel("Variable distributions",
              page_sidebar(
                  sidebar = selectInput( 
                      "data_variable", 
                      "Select options below:",
                      selected = "Age",
                      data %>%
                          select(-ends_with("_TEXT")) %>%
                          select(-starts_with(rate_column_prefixes), -starts_with(tech_usage_column_prefixes)) %>%
                          colnames()
                  ),
                  plotlyOutput("data_histogram"),
              )
    )

outcome_page <-
    nav_panel("Outcome distribution",
              plotlyOutput("data_outcome")
    )

correlations_page <-
    nav_panel("Correlation Matrices",
              page_sidebar(
                  sidebar = selectInput( 
                      "data_correlation", 
                      "Select variables for correlation:",
                      list(
                          "Nominal (one-hot encoding)" = "onehot",
                          "Nominal (integer encoding)" = "integer",
                          "Ordinal and numeric" = "ordinal"
                      )
                  ),
                  plotlyOutput("data_correlation"),
              ))

#---- 7. UI ----
ui <- page_navbar(
    nav_panel("Data",
              id = "data_tab",
              navset_pill_list(
                  widths = c(2, 10),
                  distributions_page,
                  outcome_page,
                  correlations_page
              )),
    nav_panel("Models", "Page B content"), 
    nav_panel("Take the survey", "Page C content"), 
    title = "Elements of AI, Anna Pylypenko & Kirill Putiatin", 
    id = "main_page",
)

#---- 8. Server ----
server <- function(input, output) {
    output$data_histogram <- renderPlotly({
        plot_fns <- list(
            "multichoice" = multichoice_histogram,
            "histogram" = histogram,
            "barplot" = bar_plot
        )
        plot <- case_when(
            is.numeric(data[[input$data_variable]]) ~ "histogram",
            input$data_variable %in% multichoice_vars ~ "multichoice",
            TRUE ~ "barplot"
        )
        ggplotly(plot_fns[[plot]](data, input$data_variable))
    })
    
    output$data_outcome <- renderPlotly({
        ggplotly(data %>%
            mutate(
                Country = fct_lump_n(Country, n = 20) %>%
                    fct_recode(
                        "USA" = "United States of America",
                        "UK"  = "United Kingdom of Great Britain and Northern Ireland"
                    )
            ) %>%
            select(ConvertedCompYearly, Employment, Country) %>%
            filter(ConvertedCompYearly <= upper_bound) %>%
            ggplot(mapping = aes(x = ConvertedCompYearly, y = Country, fill = Country)) +
            geom_density_ridges() +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            coord_transform(xlim = c(0, upper_bound)) +
            theme(legend.position = "none"))
    })
    
    output$data_correlation <- renderPlotly({
        ggplotly(
            ggcorr(corr_data[[input$data_correlation]], hjust = 2, size = 2)
        )
    })
}

shinyApp(ui = ui, server = server)

