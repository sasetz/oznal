library(tidyverse)
library(infotheo)
library(tidymodels)
rm(list = ls())
set.seed(124)

#---- 1. Load data ----

# setwd("~/studium/8 semester/oznal/project")
# schema <- read_csv("data/survey_results_schema.csv", show_col_types = FALSE)
# data <- read_csv("data/survey_results_public.csv", show_col_types = FALSE)
schema <- read_csv("survey_results_schema.csv", show_col_types = FALSE)
data <- read_csv("survey_results_public.csv", show_col_types = FALSE)

dir.create("output", showWarnings = FALSE)
dir.create(file.path("output", "plots"), recursive = TRUE, showWarnings = FALSE)
View(schema)
View(data)
cat("Rows:", nrow(data), ", Cols:", ncol(data), "\n")

#---- 2. Variable inventory + data quality ----

# First, let's see what variables we're working with
data %>%
  summarise(across(everything(), list(
    class = ~ class(.x)[1],
    missing = ~ sum(is.na(.x) | .x == ""),
    n_unique = ~ n_distinct(.x, na.rm = TRUE),
    n = ~ sum(!is.na(.x) & .x != ""),
    variance = ~ var(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(class|missing|n_unique|n|variance)"
  ) %>%
  View()

# Clean the target variable by removing rows with na values.
# In the 'Employment' and 'Devtype' columns, there are many illogical values,
# such as 'unemployed' with a filled 'devtype' field. We remove all such records,
# keeping only those where the person is employed and has devtype.
data <- data %>%
  drop_na(ConvertedCompYearly) %>%
  filter(ConvertedCompYearly > 0) %>%
  filter(DevType != 'Student',
         DevType != 'Retired',
         DevType != 'Other (please specify):' ) %>%
  filter(Employment != 'Retired',
         Employment != 'Not employed',
         Employment != 'I prefer not to say' )


# After basic cleaning
data %>%
    summarise(across(everything(), list(
        class = ~ class(.x)[1],
        missing = ~ sum(is.na(.x) | .x == ""),
        n_unique = ~ n_distinct(.x, na.rm = TRUE),
        n = ~ sum(!is.na(.x) & .x != ""),
        variance = ~ var(.x, na.rm = TRUE)
    ))) %>%
    pivot_longer(
        everything(),
        names_to = c("variable", ".value"),
        names_pattern = "(.*)_(class|missing|n_unique|n|variance)"
    ) %>%
View()


# COLUMNS OVERVIEW
# ---

# The dataset is quite messy, as it is a survey with a lot of open-ended
# questions, multi-choice, rating, and matrix questions. We’ve observed that
# many columns have more than 50% missing values, making it unwise to handle them.
# Therefore, we will focus on columns with less than 50% missing data. The variables
# can be grouped into rough groups on how they can be handled:
# - drop_columns: open-ended questions, ResponseId and CompTotal. Reasoning for removing CompTotal is that we will use ConvertedCompYearly instead, which is directly calculated from CompTotal
# - multifactor_columns: ordinal variables that need splitting into multiple features
# - rate_column_prefixes: prefixes for rating questions
# - multichoice_columns: multi-choice questions that need splitting into multiple features
# - tech_usage_column_prefixes: prefixes for columns that indicate the usage of a certain technology. These require substantial cleaning, since they are most likely to contain a lot of noise
# - double_multichoice_prefixes: multi-choice questions that will be converted as regular multi-choices, but each option will be an ordinal feature. See further sections for more info

# Our aim is to predict the total annual salary based on various predictors
# and identify the most significant ones. Since we have 150 columns, we
# first logically select 37 columns that are likely to influence ConvertedCompYearly.
# The survey contains many different types of questions, so some of them may not be
# relevant.
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

# selected logically columns
selected_features <- c(nominal_columns, ordinal_columns, numeric_columns,tech_usage_column_prefixes,"ConvertedCompYearly")

data_filtered <- data %>%
  select(all_of(selected_features))

cat("Rows:", nrow(data_filtered), ", Cols:", ncol(data_filtered), "\n")

# A check that we don't miss anything
data_filtered %>%
  summarise(across(everything(), list(
    class = ~ class(.x)[1],
    missing = ~ sum(is.na(.x) | .x == ""),
    n_unique = ~ n_distinct(.x, na.rm = TRUE),
    n = ~ sum(!is.na(.x) & .x != ""),
    variance = ~ var(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(class|missing|n_unique|n|variance)"
  ) %>%
  View()

# We see that our dataset have mostly character columns and only 5 numeric
table(sapply(data_filtered, class))


# Helper functions
view_distinct <- function(x, variable) {
    x %>%
        group_by(.data_filtered[[variable]]) %>%
        summarise(count = n()) %>%
        mutate(value = .data_filtered[[variable]]) %>%
        select(!all_of(variable)) %>%
        distinct()
}

plot_distinct <- function(x) {
    x %>%
        mutate(
            label = replace_na(as.character(value), "(NA)"),
            label = fct_reorder(label, count, .desc = TRUE)
        ) %>%
        ggplot(aes(x = label, y = count, fill = label == "(NA)")) +
        geom_col(show.legend = FALSE) +
        scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "tomato")) +
        labs(
            title    = paste("Distribution of the variable"),
            subtitle = "NAs shown in red",
            x        = "Value",
            y        = "Count"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

view_stats <- function(x, variable) {
    x %>%
        summarise(
            n = sum(!is.na(.data_filtered[[variable]])),
            missing = sum(is.na(.data_filtered[[variable]])),
            mean = mean(as.numeric(.data_filtered[[variable]]), na.rm = TRUE),
            median = median(as.numeric(.data_filtered[[variable]]), na.rm = TRUE),
            p25 = quantile(as.numeric(.data_filtered[[variable]]), 0.25, na.rm = TRUE),
            p75 = quantile(as.numeric(.data_filtered[[variable]]), 0.75, na.rm = TRUE)
        ) %>%
        View()
}

view_explode_distinct <- function(x, variable) {
    x %>%
        select(all_of(variable)) %>%
        filter(!is.na(.data_filtered[[variable]])) %>%
        separate_longer_delim(all_of(variable), delim = regex(";\\s*")) %>%
        group_by(.data_filtered[[variable]]) %>%
        summarise(count = n()) %>%
        arrange(.data_filtered[[variable]]) %>%
        distinct() %>%
        View()
}


# The data set contains a lot of categorical variables. Let's inspect them
# using simple histograms and save them to disk for easy retrieval

histogram <- function(x, name) {
  p <- ggplot(mapping = aes(x = x))

  if (is.numeric(x)) {
    p <- p + geom_histogram(fill = "steelblue", bins = 50)
  } else {
    p <- p + geom_bar(fill = "grey40")
  }

  p + theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     labs(title = paste("Distribution of", name), x = name)
}

hist_results <- data %>%
    select(all_of(nominal_columns), all_of(ordinal_columns)) %>%
  imap(~histogram(.x, .y))
walk(names(hist_results), function(v) {
    p <- hist_results[[v]]
    if (is.null(p)) return(NULL)
    ggsave(
        filename = file.path("output", "plots", paste0("hist_", v, ".png")),
        plot = p,
        width = 9,
        height = 6,
        dpi = 140
    )
})
# These plots tell us the following: there are some missing values, there
# are high-cardinal variables (Country) and a lot of those variables
# are dominated by one or two values


# Distribution of target variable
summary(data_filtered$ConvertedCompYearly)

# There are outliers
ggplot(data_filtered, aes(x = ConvertedCompYearly)) +
  geom_histogram(fill = "steelblue", bins = 1000) +
  coord_cartesian(xlim = c(0, 500000)) +
  theme_minimal() +
  labs(
    title = "Distribution of Yearly Compensation",
    subtitle = "Zoomed to $0 - $500k to handle outliers",
    x = "Salary (USD)",
    y = "Count"
  )

lower_bound <- quantile(data_filtered$ConvertedCompYearly, 0.025, na.rm = TRUE)
upper_bound <- quantile(data_filtered$ConvertedCompYearly, 0.975, na.rm = TRUE)

data_cleaned <- data_filtered %>%
  filter(ConvertedCompYearly >= lower_bound & ConvertedCompYearly <= upper_bound)
summary(data_cleaned$ConvertedCompYearly)

ggplot(data_cleaned, aes(x = ConvertedCompYearly)) +
  geom_histogram(fill = "steelblue", bins = 15) +
  theme_minimal() +
  labs(
    title = "Distribution of Yearly Compensation",
    subtitle = "without outliers",
    x = "Salary (USD)",
    y = "Count"
  )

#-- Clean outliers in numeric values
# We see unusually large number of tools and years of code and workexp.
summary(data_cleaned$WorkExp)
summary(data_cleaned$YearsCode)
summary(data_cleaned$ToolCountWork)

upper_bound_w <- quantile(data_filtered$WorkExp, 0.99, na.rm = TRUE)
upper_bound_w
upper_bound_y <- quantile(data_filtered$YearsCode, 0.99, na.rm = TRUE)
upper_bound_y
upper_bound_t <- quantile(data_filtered$ToolCountWork, 0.98, na.rm = TRUE)
upper_bound_t
data_cleaned <- data_cleaned %>%
  mutate(
    WorkExp = if_else(WorkExp > upper_bound_w, upper_bound_w, WorkExp),
    YearsCode = if_else(YearsCode > upper_bound_y, upper_bound_y, YearsCode),
    ToolCountWork = if_else(ToolCountWork > upper_bound_t, upper_bound_t, ToolCountWork)
  )
# After
summary(data_cleaned$WorkExp)
summary(data_cleaned$YearsCode)
summary(data_cleaned$ToolCountWork)


ggplot(data_cleaned, aes(x = WorkExp)) +
  geom_histogram(fill = "steelblue", bins = 15) +
  theme_minimal() +
  labs(
    title = "Distribution of WorkExp",
    subtitle = "without outliers",
    x = "WorkExp",
    y = "Count"
  )
ggplot(data_cleaned, aes(x = YearsCode)) +
  geom_histogram(fill = "steelblue", bins = 15) +
  theme_minimal() +
  labs(
    title = "Distribution of YearsCode",
    subtitle = "without outliers",
    x = "YearsCode",
    y = "Count"
  )
ggplot(data_cleaned, aes(x = ToolCountWork)) +
  geom_histogram(fill = "steelblue", bins = 15) +
  theme_minimal() +
  labs(
    title = "Distribution of ToolCountWork",
    subtitle = "without outliers",
    x = "ToolCountWork",
    y = "Count"
  )

cat("Rows:", nrow(data_cleaned), ", Cols:", ncol(data_cleaned), "\n")


# Let's start a recipe. We'll start by assigning roles to our variables, which
# will help us to handle them later

roles_recipe <- recipe(
    data = data_cleaned,
    formula = ConvertedCompYearly ~ .
) %>%
    add_role(all_of(nominal_columns), new_role = "nominal") %>%
    add_role(all_of(ordinal_columns), new_role = "ordinal") %>%
    add_role(all_of(numeric_columns), new_role = "numeric") %>%
    add_role(starts_with(tech_usage_column_prefixes), new_role = "tech") %>%
    prep()


# Let's convert text and deal with missing values
data_cleaned %>%
  summarise(across(everything(), list(
    class = ~ class(.x)[1],
    missing = ~ sum(is.na(.x) | .x == ""),
    n_unique = ~ n_distinct(.x, na.rm = TRUE),
    n = ~ sum(!is.na(.x) & .x != ""),
    variance = ~ var(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(class|missing|n_unique|n|variance)"
  ) %>%
  View()
# NOMINAL COLUMNS
# ---

# MainBranch and Age are the only variables without missing values.
# One-by-one, let's handle missing values. For some of them, NA means an empty
# answer, not applicable, or "I prefer not to say". For some of them, we can
# fill NA with the mode.

# ICorPM is transformed into a IsPM variable. ICorPM has only 2 options:
# "People manager" and "Individual contributor". The rest of the options are NA,
# with the mode being the contributor. It doesn't make much sense to add another
# value to the variable, so we can effectively convert this variable to a binary
# one. This could be potentially useful information, judging by the domain.

# DevType doesn't have an additional free-text variable to work with, so it is
# logical to group the "Other" option with the "Prefer not to say" (missing)
# values.

# EdLevel is imputed with the least possible value. This variable could derive
# an ordinal feature that indicated the highest education level, but it would
# lose the information about the field of study in higher education. Another
# feature could be engineered, indicating the presence of a *higher* education.

# Currency and Country are put on threshold of 1%. They are high cardinal
# variables, meaning there would be a lot of noise for the model if we keep all
# 100+ of them in encoding

# The rest of the variables are imputed with new "Missing" values, then their
# distinct options are renamed to better fit the encoding.

# Get all distinct values of a column, then print it into console in a format
# to be inserted into fct_recode(). Helpful for quick option renaming

data_cleaned$ICorPM %>%
    factor() %>%
    levels() %>%
    map_chr(~ paste0('"test" = "', .x, '",')) %>%
    writeLines()

nominal_recipe <- roles_recipe %>%
    step_factor2string(has_role("nominal")) %>%
    step_impute_mode(ICorPM, RemoteWork) %>%
    step_mutate(
        EdLevel = replace_na(EdLevel, "Primary/elementary school")
    ) %>%
    step_unknown(Industry, new_level = "None") %>%
    step_unknown(Country, new_level = "Unknown") %>%
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
    step_other(Country, threshold = 0.01, other = "Other") %>%
    prep()


# ORDINAL COLUMNS
# ---

ordinal_recipe <- nominal_recipe %>%
    step_mutate(
        Age = factor(Age, ordered = TRUE),
        Age = Age %>% fct_relevel(c("Prefer not to say",
                            "18-24 years old",
                            "25-34 years old",
                            "35-44 years old",
                            "45-54 years old",
                            "55-64 years old",
                            "65 years or older"))
    ) %>%
    step_unknown(OrgSize, new_level = "None") %>%
    step_mutate(
        OrgSize = OrgSize %>%
            factor(ordered = TRUE) %>%
            fct_recode("None" = "I don’t know") %>%
            fct_na_value_to_level("None") %>%
            fct_relevel(c("None",
                                    "Just me - I am a freelancer, sole proprietor, etc.",
                                    "Less than 20 employees",
                                    "20 to 99 employees",
                                    "100 to 499 employees",
                                    "500 to 999 employees",
                                    "1,000 to 4,999 employees",
                                    "5,000 to 9,999 employees",
                                    "10,000 or more employees")
                                  )
    ) %>%
    step_mutate(
        AIThreat = AIThreat %>%
            factor(ordered = TRUE) %>%
            fct_recode("No" = "I'm not sure") %>%
            fct_na_value_to_level("No") %>%
            fct_relevel(c("No",
                          "Yes"))
    ) %>%
    step_mutate(
        SOAccount = SOAccount %>%
            factor(ordered = TRUE) %>%
            fct_na_value_to_level("Not sure/can't remember") %>%
            fct_relevel(c("No",
                          "Not sure/can't remember",
                          "Yes"))
    ) %>%
    step_mutate(
        SOVisitFreq = SOVisitFreq %>%
            factor(ordered = TRUE) %>%
            fct_na_value_to_level("Infrequently, less than once per year") %>%
            fct_relevel(c("Infrequently, less than once per year",
                          "Less than once every 2 - 3 months",
                          "Less than once per month or monthly",
                          "A few times per month or weekly",
                          "A few times per week",
                          "Daily or almost daily",
                          "Multiple times per day"))
    ) %>%
    step_mutate(
        SODuration = SODuration %>%
            factor(ordered = TRUE) %>%
            fct_na_value_to_level("I don't use Stack Overflow") %>%
            fct_relevel(c("I don't use Stack Overflow",
                          "Less than one year",
                          "Between 1 and 3 years",
                          "Between 3 and 5 years",
                          "Between 5 and 10 years",
                          "Between 10 and 15 years",
                          "More than 15 years, or since Stack Overflow started in 2008"))
    ) %>%
    step_mutate(
        AISelect = AISelect %>%
            factor(ordered = TRUE) %>%
            fct_na_value_to_level("Yes, I use AI tools monthly or infrequently") %>%
            fct_relevel(c("No, and I don't plan to",
                          "No, but I plan to soon",
                          "Yes, I use AI tools monthly or infrequently",
                          "Yes, I use AI tools weekly",
                          "Yes, I use AI tools daily"))
    ) %>%
    step_mutate(
        AISent = AISent %>%
            factor(ordered = TRUE) %>%
            fct_na_value_to_level("Indifferent") %>%
            fct_relevel(c("Very unfavorable",
                          "Unfavorable",
                          "Unsure",
                          "Indifferent",
                          "Favorable",
                          "Very favorable"))
    ) %>%
    step_mutate(
        AIAcc = AIAcc %>%
            factor(ordered = TRUE) %>%
            fct_na_value_to_level("Neither trust nor distrust") %>%
            fct_relevel(c("Highly distrust",
                          "Highly trust",
                          "Neither trust nor distrust",
                          "Somewhat distrust",
                          "Somewhat trust"))
    ) %>%
    step_mutate(
        AIComplex = AIComplex %>%
            factor(ordered = TRUE) %>%
            fct_na_value_to_level("I don't use AI tools for complex tasks / I don't know") %>%
            fct_relevel(c("Very poor at handling complex tasks",
                          "Bad at handling complex tasks",
                          "I don't use AI tools for complex tasks / I don't know",
                          "Neither good or bad at handling complex tasks",
                          "Good, but not great at handling complex tasks",
                          "Very well at handling complex tasks"))
    ) %>%
    step_mutate(
        AIAgents = AIAgents %>%
            factor(ordered = TRUE) %>%
            fct_na_value_to_level("No, and I don't plan to") %>%
            fct_relevel(c("No, and I don't plan to",
                          "No, but I plan to",
                          "No, I use AI exclusively in copilot/autocomplete mode",
                          "Yes, I use AI agents at work daily",
                          "Yes, I use AI agents at work monthly or infrequently",
                          "Yes, I use AI agents at work weekly"))
    ) %>%
    step_mutate(
        AIAgentChange = AIAgentChange %>%
            factor(ordered = TRUE) %>%
            fct_na_value_to_level("Not at all or minimally") %>%
            fct_relevel(c("Not at all or minimally",
                          "No, but my development work has changed somewhat due to non-AI factors",
                          "No, but my development work has significantly changed due to non-AI factors",
                          "Yes, somewhat",
                          "Yes, to a great extent"))
    ) %>%
    step_impute_median(JobSat) %>%
    prep()

# NUMERIC COLUMNS
# ---


numeric_recipe <- ordinal_recipe %>%
    step_impute_median(has_role("numeric")) %>%
    prep()

numeric_recipe %>%
    step_rm(-has_role("numeric")) %>%
    prep() %>%
    bake(new_data = NULL) %>%
    View()


data_cleaned %>%
    select(WorkExp, Age) %>%
    ggplot(mapping = aes(x = WorkExp, y = Age, fill = Age)) +
    geom_violin() +
    theme_minimal()

# MULTIFACTOR COLUMNS
# ---

# These columns contain answers to questions that can be interpreted as multiple
# features. Let's determine the relevant ones and split them

library(GGally)
numeric_recipe %>%
    step_rm(-has_role("ordinal"), -has_role("numeric"), -has_role("outcome")) %>%
    step_ordinalscore(has_role("ordinal"), -JobSat) %>%
    step_integer(has_role("nominal")) %>%
    prep() %>%
    bake(new_data = NULL) %>%
    ggcorr()

nominal_columns

numeric_recipe %>%
    step_rm(-has_role("outcome")) %>%
    step_naomit(has_role("outcome")) %>%
    step_dummy(has_role("nominal"), one_hot = TRUE) %>%
    prep() %>%
    bake(new_data = NULL)


# ENCODING ALL FEATURES
# ---
# Apply one-hot encoding to all prepared nominal variables


final_recipe <- numeric_recipe %>%
    step_naomit(has_role("outcome")) %>%
    step_rm(-has_role("numeric"), -has_role("ordinal"), -has_role("nominal"), -has_role("outcome")) %>%
    step_dummy(has_role("nominal"), one_hot = TRUE) %>%
    step_ordinalscore(has_role("ordinal"), -c("JobSat")) %>%
    prep()

final_data <- final_recipe %>%
    bake(new_data = NULL)

View(final_data)

final_data %>%
  summarise(across(everything(), list(
    class = ~ class(.x)[1],
    missing = ~ sum(is.na(.x) | .x == ""),
    n_unique = ~ n_distinct(.x, na.rm = TRUE),
    n = ~ sum(!is.na(.x) & .x != ""),
    variance = ~ var(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(class|missing|n_unique|n|variance)"
  ) %>%
  View()
