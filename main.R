library(tidyverse)
library(infotheo)
library(GGally)
library(tidymodels)
rm(list = ls())
set.seed(124)

#---- 1. Load data ----

setwd("~/studium/8 semester/oznal/project")
schema <- read_csv("data/survey_results_schema.csv", show_col_types = FALSE)
data <- read_csv("data/survey_results_public.csv", show_col_types = FALSE)

dir.create("output", showWarnings = FALSE)
dir.create(file.path("output", "plots"), recursive = TRUE, showWarnings = FALSE)

cat("Rows:", nrow(data), ", Cols:", ncol(data), "\n")

#---- 2. EDA and Data Cleaning ----

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

# Define a few helper functions for exploring variables, their distributions

view_distinct <- function(x, variable) {
    x %>%
        group_by(.data[[variable]]) %>%
        summarise(count = n()) %>%
        mutate(value = .data[[variable]]) %>%
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
            n = sum(!is.na(.data[[variable]])),
            missing = sum(is.na(.data[[variable]])),
            mean = mean(as.numeric(.data[[variable]]), na.rm = TRUE),
            median = median(as.numeric(.data[[variable]]), na.rm = TRUE),
            p25 = quantile(as.numeric(.data[[variable]]), 0.25, na.rm = TRUE),
            p75 = quantile(as.numeric(.data[[variable]]), 0.75, na.rm = TRUE)
        ) %>%
        View()
}
view_explode_distinct <- function(x, variable) {
    x %>%
        select(all_of(variable)) %>%
        filter(!is.na(.data[[variable]])) %>%
        separate_longer_delim(all_of(variable), delim = regex(";\\s*")) %>%
        group_by(.data[[variable]]) %>%
        summarise(count = n()) %>%
        arrange(.data[[variable]]) %>%
        distinct() %>%
        View()
}

# COLUMNS OVERVIEW
# ---

# The dataset is quite messy, as it is a survey with a lot of open-ended
# questions, multi-choice, rating, and matrix questions. A lot of them are
# missing for various reasons. The variables can be grouped into rough groups
# on how they can be handled. There are a few worth a special mention:
# - drop_columns: open-ended questions, ResponseId and CompTotal. Reasoning for removing CompTotal is that we will use ConvertedCompYearly instead, which is directly calculated from CompTotal
# - multifactor_columns: ordinal variables that need splitting into multiple features
# - rate_column_prefixes: prefixes for rating questions
# - multichoice_columns: multi-choice questions that need splitting into multiple features
# - tech_usage_column_prefixes: prefixes for columns that indicate the usage of a certain technology. These require substantial cleaning, since they are most likely to contain a lot of noise
# - double_multichoice_prefixes: multi-choice questions that will be converted as regular multi-choices, but each option will be an ordinal feature. See further sections for more info

drop_columns <- c("ResponseId", "CompTotal", "AIExplain", "AIOpen",
                  "AIAgentExtWrite", "AIAgentKnowWrite", "AIAgentOrchWrite",
                  "AIAgentObsWrite")
nominal_columns <- c("MainBranch", "EdLevel", "Employment",
                     "DevType", "ICorPM", "RemoteWork", "TechEndorseIntro",
                     "Industry", "Country", "Currency")
ordinal_columns <- c("Age", "OrgSize", "AIThreat", "SOAccount", "SOVisitFreq",
                     "SODuration", "SOPartFreq", "SOComm", "SOFriction",
                     "AISelect", "AISent", "AIAcc", "AIComplex", "AIAgents",
                     "JobSat", "AIAgentChange")
numeric_columns <- c("WorkExp", "YearsCode", "ToolCountWork",
                     "ToolCountPersonal")
multifactor_columns <- c("LearnCodeChoose", "LearnCodeAI", "PurchaseInfluence",
                         "NewRole", "LearnCodeChoose")
rate_column_prefixes <- c("TechEndorse_", "JobSatPoints_", "TechOppose_",
                          "SO_Actions_")
multichoice_columns <- c("EmploymentAddl", "LearnCode", "AILearnHow",
                         "SO_Dev_Content", "AIFrustration", "AIAgent_Uses",
                         "AgentUsesGeneral", "AIAgentKnowledge",
                         "AIAgentOrchestration", "AIAgentObserveSecure",
                         "AIAgentExternal", "AIHuman")
tech_usage_column_prefixes <- c("Language", "Database", "Platform", "Webframe", 
                                "DevEnvs", "AIModels", "SOTags", "CommPlatform",
                                "OfficeStackAsync", "DevEnv", "OpSys", "OfficeStack")
# this can be handled the following way: take all distinct values from these
# answers, convert them to columns and assign an ordinal rating, from 0
# (don't plan to use AI) to 5 (currently mostly using AI)
double_multichoice_prefixes <- c("AITool", "AIAgentImpact", "AIAgentChallenges")

# A check that we don't miss anything and don't have bad column names
data %>%
    select(-starts_with(rate_column_prefixes),
           -starts_with(tech_usage_column_prefixes),
           -starts_with(double_multichoice_prefixes),
           -all_of(c(
               drop_columns,
               nominal_columns,
               ordinal_columns,
               numeric_columns,
               multifactor_columns,
               multichoice_columns
           ))) %>%
    View()

# Inspecting the ConvertedCompYearly variable, we can see that it contains a lot
# of missing values, which don't mean a lot for our task. Let's drop these rows,
# so we end up with a much smaller dataset length (about 50% of original 49k)

data <- data %>%
    drop_na(ConvertedCompYearly) %>%
    filter(ConvertedCompYearly > 0)

# The data set contains a lot of categorical variables. Let's inspect them
# using simple histograms and save them to disk for easy retrieval

histogram <- function(x) {
    ggplot(mapping = aes(x = x)) +
        geom_bar() +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

hist_results <- data %>%
    select(all_of(nominal_columns), all_of(ordinal_columns), ConvertedCompYearly) %>%
    map(histogram)

# Let's see the distributions of yearly compensations in relation to employment
# status and country. We have to cut the outliers first, as there are quite a
# few of them, and group the countries into a factor of 20, so that they don't
# blow out the plot. These distributions tell us that the ConvertedCompYearly
# is heavily dependent on the country, and they are just different distributions
# altogether. This will be very important for modeling later
library(ggridges)
high_cutoff <- quantile(
    data$ConvertedCompYearly,
    probs=c(.25, .75)
    )[2] + 1.5 * IQR(data$ConvertedCompYearly)
data %>%
    #filter(Employment == "Employed" | Employment == "Independent contractor, freelancer, or self-employed") %>%
    mutate(
        Country = fct_lump_n(Country, n = 20) %>%
            fct_recode(
                "USA" = "United States of America",
                "UK"  = "United Kingdom of Great Britain and Northern Ireland"
            )
           ) %>%
    select(ConvertedCompYearly, Employment, Country) %>%
    filter(ConvertedCompYearly <= high_cutoff) %>%
    ggplot(mapping = aes(x = ConvertedCompYearly, y = Country, fill = Country)) +
    geom_density_ridges() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~Employment) +
    coord_transform(xlim = c(0, high_cutoff))

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

# These plots tell us the following: there are a lot of missing values, there
# are high-cardinal variables (Country, Currency) and a lot of those variables
# are dominated by one or two values

# Let's also map the categorical variables using violin plots in how they
# compare to the ConvertedCompYearly

violin <- function(x) {
    ggplot(mapping = aes(x = x, y = data$ConvertedCompYearly, fill = x)) +
        geom_violin() +
        geom_jitter(color="black", size=0.2, alpha=0.6) +
        theme_minimal() +
        theme(axis.text.x = element_blank()) +
        coord_transform(ylim = quantile(data$ConvertedCompYearly, .99, na.rm = TRUE) * c(0, 1))
}

violin_results <- data %>%
    select(all_of(nominal_columns), all_of(ordinal_columns)) %>%
    map(violin)

walk(names(violin_results), function(v) {
    p <- violin_results[[v]]
    ggsave(
        filename = file.path("output", "plots", paste0("violin_", v, ".png")),
        plot = p,
        width = 9,
        height = 6,
        dpi = 140
    )
})

# Let's find out categorical variables' mutual information with the outcome.

data %>%
    select(all_of(nominal_columns), all_of(ordinal_columns), ConvertedCompYearly) %>%
    filter(!is.na(ConvertedCompYearly)) %>%
    summarise(across(!ConvertedCompYearly,
                     .fns = list(mi = ~ infotheo::mutinformation(
                         infotheo::discretize(.x),
                         infotheo::discretize(ConvertedCompYearly))))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "mutual_information") %>%
    View()

# Let's start a recipe. We'll start by assigning roles to our variables, which
# will help us to handle them later

roles_recipe <- recipe(
    data = data,
    formula = ConvertedCompYearly ~ .
) %>%
    add_role(all_of(nominal_columns), new_role = "nominal") %>%
    add_role(all_of(ordinal_columns), new_role = "ordinal") %>%
    add_role(all_of(numeric_columns), new_role = "numeric") %>%
    add_role(all_of(multifactor_columns), new_role = "multifactor") %>%
    add_role(starts_with(rate_column_prefixes), new_role = "rate") %>%
    add_role(all_of(multichoice_columns), new_role = "multichoice") %>%
    add_role(starts_with(tech_usage_column_prefixes), new_role = "tech") %>%
    add_role(starts_with(double_multichoice_prefixes), new_role = "double_multichoice") %>%
    step_rm(all_of(drop_columns)) %>%
    prep()

# NOMINAL COLUMNS
# ---

# ResponseId, MainBranch and Age are the only variables without missing values.
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

data$ICorPM %>%
    factor() %>%
    levels() %>%
    map_chr(~ paste0('"test" = "', .x, '",')) %>%
    writeLines()

nominal_recipe <- roles_recipe %>%
    step_factor2string(has_role("nominal")) %>%
    step_impute_mode(ICorPM, RemoteWork) %>%
    step_mutate(
        Employment = replace_na(Employment, "I prefer not to say"),
        DevType = coalesce(na_if(DevType, "Other (please specify):"), "I prefer not to say"),
        EdLevel = replace_na(EdLevel, "Primary/elementary school")
    ) %>%
    step_unknown(Industry, TechEndorseIntro, new_level = "None") %>%
    step_unknown(Country, Currency, new_level = "Unknown") %>%
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
                                "Unknown" = "I prefer not to say",
                                "Independent" = "Independent contractor, freelancer, or self-employed",
                                "Unemployed" = "Not employed",
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
                             "Retired" = "Retired",
                             "Executive" = "Senior executive (C-suite, VP, etc.)",
                             "Student" = "Student",
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
        ),
        TechEndorseIntro = fct_recode(TechEndorseIntro,
                                      "Project" = "Personal Project",
                                      "School" = "School",
                                      "Work" = "Work",
                                      "None" = "None"
        ),
    ) %>%
    # convert ICorPM to IsPM (is People Manager)
    step_rename(IsPM = ICorPM) %>%
    step_mutate(IsPM = if_else(IsPM == "People manager", 1L, 0L)) %>%
    step_mutate(
        # get first 3 letters of the currency, since that code will indicate the currency uniquely
        Currency = fct_relabel(Currency, function(x) if_else(x != "Unknown", substr(x, start = 1, stop = 3), x))
    ) %>%
    step_other(Country, Currency, threshold = 0.01, other = "Other") %>%
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
            fct_na_value_to_level("None") %>%
            fct_relevel(c("None",
                                    "I don’t know",
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
            fct_na_value_to_level("No") %>%
            fct_relevel(c("No",
                          "I'm not sure",
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
        SOPartFreq = SOPartFreq %>%
            factor(ordered = TRUE) %>%
            fct_na_value_to_level("I have never participated in Q&A on Stack Overflow") %>%
            fct_relevel(c("I have never participated in Q&A on Stack Overflow",
                          "Infrequently, less than once per year",
                          "Less than once every 2 - 3 months",
                          "Less than once per month or monthly",
                          "A few times per month or weekly",
                          "A few times per week",
                          "Daily or almost daily",
                          "Multiple times per day"))
    ) %>%
    step_mutate(
        SOComm = SOComm %>%
            factor(ordered = TRUE) %>%
            fct_na_value_to_level("No, not at all") %>%
            fct_relevel(c("No, not at all",
                          "No, not really",
                          "Not sure",
                          "Neutral",
                          "Yes, somewhat",
                          "Yes, definitely"))
    ) %>%
    step_mutate(
        SOFriction = SOFriction %>%
            factor(ordered = TRUE) %>%
            fct_na_value_to_level("No, not at all") %>%
            fct_relevel(c("Rarely, almost never",
                          "About half of the time",
                          "I don't use AI or AI-enabled tools",
                          "Less than half of the time",
                          "More than half the time"))
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

# Numeric columns are simplest to replace with their medians, we'll come back to
# them later

numeric_recipe <- ordinal_recipe %>%
    step_impute_median(has_role("numeric")) %>%
    prep()

# FILTERING VARIABLES
# ---

# Let's conduct simple filtering for NAs first. We'll get rid of all variables
# that have more than 50% of NAs in their 

data %>%
    summarise_all(~ sum(is.na(.))) %>%
    View()

data %>%
    select(WorkExp, Age) %>%
    ggplot(mapping = aes(x = WorkExp, y = Age, fill = Age)) +
    geom_violin() +
    theme_minimal()

# MULTIFACTOR COLUMNS
# ---

# These columns contain answers to questions that can be interpreted as multiple
# features. Let's determine the relevant ones and split them

data %>%
    select(ConvertedCompYearly) %>%
    summarise(across(everything(), ~ sum(!is.na(.)))) %>%
    View()

#---- 3. Linear regression ----

# First of all, let's check the correlation between the variables. We don't want
# collinear variables. Starting from ordinal and numeric variables, let's build
# correlation matrices

# The following correlation matrix eliminates a few variables:
# - Age, WorkExp and YearsCode are strongly correlated, we'll have to take one
# - SO prefixed variables (StackOverflow) are all correlated. This is probably due to value imputation
# - Same thing for AI related questions (except AIThreat)
numeric_recipe %>%
    step_rm(-has_role("ordinal"), -has_role("numeric"), -has_role("outcome")) %>%
    step_ordinalscore(has_role("ordinal"), -JobSat) %>%
    step_integer(has_role("nominal")) %>%
    step_naomit(has_role("outcome")) %>%
    prep() %>%
    bake(new_data = NULL) %>%
    ggcorr()

nominal_columns

numeric_recipe %>%
    step_rm(-Currency, -has_role("outcome")) %>%
    step_naomit(has_role("outcome")) %>%
    step_dummy(has_role("nominal"), one_hot = TRUE) %>%
    prep() %>%
    bake(new_data = NULL) %>%
    ggcorr()

mi_currency_country <- numeric_recipe %>%
    step_naomit(has_role("outcome")) %>%
    prep() %>%
    bake(new_data = NULL) %>%
    select(Country, Currency, ConvertedCompYearly)

mi_result <- mi_currency_country %>%
    # Joint probability p(x, y)
    count(Country, Currency, name = "n_joint") %>%
    mutate(p_joint = n_joint / sum(n_joint)) %>%
    
    # Marginal probability p(x)
    group_by(Country) %>%
    mutate(p_x = sum(p_joint)) %>%
    
    # Marginal probability p(y)
    group_by(Currency) %>%
    mutate(p_y = sum(p_joint)) %>%
    ungroup() %>%
    
    # MI contribution per cell: p(x,y) * log(p(x,y) / p(x)*p(y))
    mutate(mi_cell = p_joint * log2(p_joint / (p_x * p_y)))

mutual_information <- sum(mi_result$mi_cell)
cat("Mutual Information:", round(mutual_information, 4), "bits\n")

mi_result %>%
    mutate(
        label = paste0(
            "p=", round(p_joint, 3),
            "\nmi=", round(mi_cell, 4)
        )
    ) %>%
    ggplot(aes(x = Country, y = Currency)) +
    
    # Tile color = MI contribution (diverging: negative = suppression, positive = association)
    geom_tile(aes(fill = mi_cell), color = "white", linewidth = 1.2) +
    
    # Annotate each cell
    geom_text(aes(label = label), size = 3.5, color = "white", fontface = "bold") +
    
    scale_fill_gradient2(
        low  = "#2166ac",   # negative MI (suppression)
        mid  = "#f7f7f7",
        high = "#b2182b",   # positive MI (strong association)
        midpoint = 0,
        name = "MI\ncontribution\n(bits)"
    ) +
    
    labs(
        title    = glue::glue("Mutual Information: {round(mutual_information, 4)} bits"),
        subtitle = "Cell color = MI contribution  |  p = joint probability",
        x = "Variable X",
        y = "Variable Y",
        caption  = "MI(X,Y) = Σ p(x,y) · log₂[ p(x,y) / (p(x)·p(y)) ]"
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
        plot.title    = element_text(face = "bold"),
        panel.grid    = element_blank(),
        legend.position = "right"
    )

# ENCODING ALL FEATURES
# ---
# Apply one-hot encoding to all prepared nominal variables


final_recipe <- numeric_recipe %>%
    step_naomit(has_role("outcome")) %>%
    step_rm(-has_role("numeric"), -has_role("ordinal"), -has_role("nominal"), -has_role("outcome")) %>%
    step_dummy(has_role("nominal"), one_hot = TRUE) %>%
    step_ordinalscore(has_role("ordinal"), -c("JobSat")) %>%
    prep()

lr_recipe <- numeric_recipe %>%
    step_naomit(has_role("outcome")) %>%
    step_rm(-has_role("numeric"), -has_role("ordinal"), -has_role("nominal"), -has_role("outcome")) %>%
    step_rm(Country, Age, YearsCode, starts_with("SO"), starts_with("AI")) %>%
    step_dummy(has_role("nominal"), one_hot = TRUE) %>%
    step_ordinalscore(has_role("ordinal"), -c("JobSat")) %>%
    prep()

lr_recipe %>%
    bake(new_data = NULL) -> clean_data
    #View()


model <- linear_reg()
lm_fit <-
    model %>%
    fit(ConvertedCompYearly ~ ., data = clean_data); lm_fit

View(tidy(lm_fit))
